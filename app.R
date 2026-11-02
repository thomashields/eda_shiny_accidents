
# -----------------------------------------
# South Dakota Traffic Fatalities
# -----------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(plotly)
library(viridis)
library(tigris)

options(tigris_use_cache = TRUE)

# -----------------------------------------
# Load data
# -----------------------------------------

sd_accidents <- readRDS("sd_accidents.rds")
sd_counties <- readRDS("sd_counties.rds")
sd_county_summary <- readRDS("sd_county_summary.rds")

# Make GEOID columns consistent
sd_accidents <- sd_accidents %>%
  mutate(GEOID = as.character(GEOID))

sd_counties <- sd_counties %>%
  mutate(GEOID = as.character(GEOID))

sd_county_summary <- sd_county_summary %>%
  mutate(GEOID = as.character(GEOID))

# -----------------------------------------
# Load interstate highways
# -----------------------------------------

interstates <- primary_secondary_roads(
  state = "SD",
  year = 2024
) %>%
  filter(RTTYP == "I") %>%
  st_transform(5070)

# -----------------------------------------
# Chart variables
# -----------------------------------------

variable_choices <- c(
  "Month" = "MONTHNAME",
  "Day of Week" = "DAY_WEEKNAME",
  "Hour" = "HOURNAME",
  "Weather" = "WEATHERNAME",
  "Lighting Conditions" = "LGT_CONDNAME",
  "Road Type" = "ROUTENAME",
  "Collision Type" = "MAN_COLLNAME",
  "Rural or Urban" = "RUR_URBNAME"
)

# -----------------------------------------
# UI
# -----------------------------------------

ui <- fluidPage(
  
  titlePanel("South Dakota Traffic Fatalities"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filters"),
      
      selectInput(
        "year",
        "Year",
        choices = c("2023", "2024"),
        selected = "2024"
      ),
      
      selectInput(
        "variable",
        "Explore",
        choices = variable_choices,
        selected = "MONTHNAME"
      ),
      
      selectInput(
        "county",
        "County",
        choices = c("All Counties", sort(sd_counties$NAME)),
        selected = "All Counties"
      )
      
    ),
    
    mainPanel(
      
      # Map
      h3("Fatalities per 100,000 Residents"),
      
      p(
        "County color shows fatality rate. Red lines show interstates. Hover over a black dot for county details."
      ),
      
      plotlyOutput("county_map", height = "500px"),
      
      hr(),
      
      # Bar chart
      h3("Fatal Crash Patterns"),
      
      p(
        "Bars show the number of fatal crashes. Select a variable and county to explore patterns."
      ),
      
      plotlyOutput("accident_plot", height = "450px")
      
    )
  )
)

# -----------------------------------------
# SERVER
# -----------------------------------------

server <- function(input, output, session) {
  
  # ---------------------------------------
  # Map data
  # ---------------------------------------
  
  map_data <- reactive({
    
    sd_counties %>%
      left_join(
        sd_county_summary %>%
          filter(YEAR == as.numeric(input$year)),
        by = "GEOID"
      ) %>%
      mutate(
        FATALITY_RATE_100K = if_else(
          !is.na(POPULATION) & POPULATION > 0,
          TOTAL_FATALITIES / POPULATION * 100000,
          NA_real_
        )
      ) %>%
      st_transform(5070)
    
  })
  
  # ---------------------------------------
  # Interactive county map
  # ---------------------------------------
  
  output$county_map <- renderPlotly({
    
    data <- map_data()
    
    # Find county centers
    county_points <- st_centroid(data)
    
    # Move centers inside their counties if necessary
    inside <- lengths(
      st_covered_by(county_points, data)
    ) > 0
    
    st_geometry(county_points)[!inside] <-
      st_geometry(
        st_point_on_surface(data[!inside, ])
      )
    
    # County hover information
    county_points$hover_text <- paste0(
      "<b>", county_points$NAME, " County</b><br>",
      "Fatalities: ",
      county_points$TOTAL_FATALITIES, "<br>",
      "Population: ",
      format(county_points$POPULATION, big.mark = ","), "<br>",
      "Fatalities per 100,000: ",
      round(county_points$FATALITY_RATE_100K, 2)
    )
    
    p <- ggplot() +
      
      # County shading
      geom_sf(
        data = data,
        aes(fill = FATALITY_RATE_100K),
        color = "white",
        linewidth = 0.3
      ) +
      
      # Interstate highways
      geom_sf(
        data = interstates,
        color = "red",
        linewidth = 1.1
      ) +
      
      # County hover dots
      geom_sf(
        data = county_points,
        aes(text = hover_text),
        color = "black",
        size = 1.3
      ) +
      
      scale_fill_viridis_c(
        option = "C",
        name = "Fatalities<br>per 100,000",
        na.value = "grey85"
      ) +
      
      labs(
        title = paste(
          "South Dakota Fatality Rates -",
          input$year
        )
      ) +
      
      theme_void() +
      
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5
        ),
        legend.position = "right"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ---------------------------------------
  # Filter accidents
  # ---------------------------------------
  
  filtered_accidents <- reactive({
    
    data <- sd_accidents %>%
      filter(YEAR == as.numeric(input$year))
    
    if (input$county != "All Counties") {
      
      selected_geoid <- sd_counties %>%
        filter(NAME == input$county) %>%
        pull(GEOID)
      
      data <- data %>%
        filter(GEOID %in% selected_geoid)
      
    }
    
    data
    
  })
  
  # ---------------------------------------
  # Prepare chart data
  # ---------------------------------------
  
  plot_data <- reactive({
    
    data <- filtered_accidents()
    variable <- input$variable
    
    validate(
      need(
        variable %in% names(data),
        paste("Missing variable:", variable)
      )
    )
    
    # Get selected variable
    values <- as.character(data[[variable]])
    
    # Remove missing, blank, and Unknown values
    values <- values[
      !is.na(values) &
        trimws(values) != "" &
        tolower(trimws(values)) != "unknown"
    ]
    
    counts <- tibble(Category = values) %>%
      count(Category, name = "Crashes")
    
    # Month order
    if (variable == "MONTHNAME") {
      
      category_order <- month.name
      
      # Day of week order
    } else if (variable == "DAY_WEEKNAME") {
      
      category_order <- sd_accidents %>%
        filter(!is.na(DAY_WEEKNAME)) %>%
        distinct(DAY_WEEK, DAY_WEEKNAME) %>%
        arrange(as.numeric(as.character(DAY_WEEK))) %>%
        pull(DAY_WEEKNAME)
      
      # Hour order
    } else if (variable == "HOURNAME") {
      
      category_order <- sd_accidents %>%
        filter(!is.na(HOURNAME)) %>%
        distinct(HOUR, HOURNAME) %>%
        arrange(as.numeric(as.character(HOUR))) %>%
        pull(HOURNAME)
      
    } else {
      
      return(counts)
      
    }
    
    # Include categories with zero crashes
    result <- tibble(Category = category_order) %>%
      left_join(counts, by = "Category") %>%
      mutate(Crashes = coalesce(Crashes, 0L))
    
    result %>%
      mutate(
        Category = factor(
          Category,
          levels = category_order
        )
      )
    
  })
  
  # ---------------------------------------
  # Bar chart
  # ---------------------------------------
  
  output$accident_plot <- renderPlotly({
    
    data <- plot_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No crash records found for this selection."
      )
    )
    
    ordered_variables <- c(
      "MONTHNAME",
      "DAY_WEEKNAME",
      "HOURNAME"
    )
    
    # Preserve chronological order for month, day, and hour
    if (input$variable %in% ordered_variables) {
      
      p <- ggplot(
        data,
        aes(
          x = Category,
          y = Crashes,
          text = paste0(
            Category,
            "<br>Fatal Crashes: ",
            Crashes
          )
        )
      ) +
        
        scale_x_discrete(
          limits = rev(levels(data$Category)),
          drop = FALSE
        )
      
    } else {
      
      # Sort other variables by crash count
      p <- ggplot(
        data,
        aes(
          x = reorder(Category, Crashes),
          y = Crashes,
          text = paste0(
            Category,
            "<br>Fatal Crashes: ",
            Crashes
          )
        )
      )
      
    }
    
    p <- p +
      
      geom_col(fill = "steelblue") +
      
      coord_flip() +
      
      labs(
        title = paste(
          names(variable_choices)[
            match(input$variable, variable_choices)
          ],
          "-",
          input$county
        ),
        x = NULL,
        y = "Number of Fatal Crashes"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          size = 15,
          face = "bold"
        )
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
}

# -----------------------------------------
# Run application
# -----------------------------------------

shinyApp(ui = ui, server = server)
