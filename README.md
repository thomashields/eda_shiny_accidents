# Overview

This project explores fatal traffic crashes in South Dakota during 2023 and 2024 using data from the Fatality Analysis Reporting System (FARS).

An interactive Shiny application allows users to examine county fatality rates and explore patterns in fatal crashes across different variables.

# Dashboard Features

## County Map

The interactive map displays all South Dakota counties.

- County shading represents fatalities per 100,000 residents.
- Black dots indicate county locations. Hover over a dot to view county statistics.
- Red lines represent interstate highways.
- The map updates when the selected year changes.

## Fatal Crash Patterns

The bar chart allows users to explore fatal crash counts by:

- Month
- Day of the week
- Hour of the day
- Weather conditions
- Lighting conditions
- Road type
- Collision type
- Rural or urban location

Users can filter the chart by year and county. County selection only affects the bar chart, not the map.

# Project Files

The project includes the following files:

| File | Description |
|:---|:---|
| `app.R` | Shiny application code |
| `data_cleaning.R` | Data cleaning and preparation script |
| `sd_accidents.rds` | Cleaned fatal crash records |
| `sd_counties.rds` | South Dakota county boundaries |
| `sd_county_summary.rds` | County-level fatality and population summaries |
| `sd_counties_pop.xlsx` | County population data |

Original FARS datasets for 2023 and 2024 are also used during data preparation.

# Requirements

The application requires R and the following packages:

```{r eval=FALSE}
install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "sf",
  "plotly",
  "viridis",
  "tigris"
))
```

# Running the Application

1. Download or clone the project repository.
2. Open the project folder in RStudio or VS Code.
3. Make sure the cleaned RDS files are in the project directory.
4. Install the required packages.
5. Open `app.R` and run the application.

Alternatively, run:

```{r eval=FALSE}
shiny::runApp()
```

An internet connection may be required to retrieve interstate highway data through the `tigris` package.

# Data Sources

- **Fatality Analysis Reporting System (FARS):** Fatal crash records for 2023 and 2024, provided by the National Highway Traffic Safety Administration (NHTSA).
- **Population Data:** South Dakota county population data used to calculate fatality rates.
- **U.S. Census TIGER/Line:** Geographic road data used to display interstate highways.

# Definitions

**Fatal Crashes:** The number of crashes involving at least one fatality.

**Fatalities per 100,000 Residents:** The number of traffic fatalities divided by the county population and multiplied by 100,000.

A fatal crash and a fatality are not necessarily the same thing. One crash can involve multiple fatalities.

# Project Purpose

The purpose of this project is to make South Dakota traffic fatality data accessible through interactive visualizations, allowing users to explore geographic differences and identify patterns in fatal crashes.
