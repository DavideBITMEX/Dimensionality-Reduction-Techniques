library(tidyverse)     
library(eurostat)      
library(FactoMineR)   
library(factoextra)   
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)  
library(patchwork) 
library(gridExtra)  
library(highcharter)
library(knitr)
library(kableExtra)
library(htmltools)
library(htmlwidgets)

data <- get_eurostat("lfsa_egan")

# Gender-Based Employment Trends
## Employment Levels by Gender
{
plot_employment_levels_by_gender <- function(
    data,
    age_filter = "Y15-64",
    citizen_filter = c("FOR", "NAT", "STLS", "NRP"),
    sex_filter = c("M", "F"),
    geo_filter = c("EU27_2020", "CH", "IS", "ME", "NO", "RS", "UK", "MK", "BA", "TR"),
    chart_title = "Employment Levels by Gender for the Working Age Population (15-64) Over Time",
    chart_subtitle = "All countries (excluding EU-major aggregates)",
    male_color = "#003399",
    female_color = "#FFCC00"
) {
  # Filter and preprocess the data
  filtered_data <- data %>%
    filter(
      age == age_filter,
      citizen %in% citizen_filter,
      sex %in% sex_filter,
      geo %in% geo_filter
    ) %>%
    mutate(
      year = year(TIME_PERIOD),
      employment_real = values * 1000  # Convert employment values to actual numbers
    )
  
  # Summarize employment by year and sex
  summarized_data <- filtered_data %>%
    group_by(year, sex) %>%
    summarise(total_employment = sum(employment_real, na.rm = TRUE)) %>%
    ungroup()
  
  # Create the Highcharter bar plot
  hchart <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = chart_title) %>%
    hc_subtitle(text = chart_subtitle) %>%
    hc_xAxis(
      categories = unique(summarized_data$year),
      title = list(text = "Year"),
      labels = list(rotation = 45)  # Rotate x-axis labels for readability
    ) %>%
    hc_yAxis(
      title = list(text = "Total Employment"),
      labels = list(format = "{value:,}")  # Format y-axis with commas
    ) %>%
    hc_plotOptions(column = list(grouping = TRUE)) %>%
    hc_tooltip(shared = TRUE, pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br>") %>%
    hc_add_series(
      data = summarized_data %>% filter(sex == "M") %>% pull(total_employment),
      name = "Male",
      color = male_color
    ) %>%
    hc_add_series(
      data = summarized_data %>% filter(sex == "F") %>% pull(total_employment),
      name = "Female",
      color = female_color
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE)
  
  return(hchart)
}
plot_employment_levels_by_gender(data)
}

## 
plot_employment_levels_by_gender(
  data,
  age_filter = "Y15-64",
  citizen_filter = c("FOR", "NAT", "STLS", "NRP"),
  sex_filter = c("M", "F"),
  geo_filter = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"
  ),
  chart_title = "Employment Levels by Gender for the Working Age Population (15-64) Over Time",
  chart_subtitle = "All EU Member States",
  male_color = "#003399",
  female_color = "#FFCC00"
)

##
plot_employment_levels_by_gender(
  data,
  age_filter = "Y15-64",
  citizen_filter = c("FOR", "NAT", "STLS", "NRP"),
  sex_filter = c("M", "F"),
  geo_filter = c("CH", "IS", "ME", "NO", "RS", "UK", "MK", "BA", "TR"),
  chart_title = "Employment Levels by Gender for the Working Age Population (15-64) Over Time",
  chart_subtitle = "non-EU Member States",
  male_color = "#003399",
  female_color = "#FFCC00"
)

## Employment Levels by Gender and Country (Working Age: 15-64)
{
  # Filter the data for working age (Y15-64), Males (M), Females (F), and citizen TOTAL
  filtered_data <- data %>%
    filter(
      age == "Y15-64",
      sex %in% c("M", "F"),
      citizen == "TOTAL"
    ) %>%
    mutate(
      year = year(TIME_PERIOD),
      employment_real = values * 1000  # Convert employment values to actual numbers
    )
  
  # Summarize employment by year, country, and sex
  summarized_data <- filtered_data %>%
    group_by(year, geo, sex) %>%
    summarise(total_employment = sum(employment_real, na.rm = TRUE)) %>%
    ungroup()
  
  # Ensure all combinations of year, geo, and sex exist, filling missing ones with zero
  summarized_data <- summarized_data %>%
    complete(year, geo, sex, fill = list(total_employment = 0))
  
  # Define the correct order of countries with EA20 and EU27_2020 first
  geo_levels <- c("EA20", "EU27_2020", setdiff(unique(summarized_data$geo), c("EA20", "EU27_2020")))
  
  # Apply the factor order to geo
  summarized_data$geo <- factor(summarized_data$geo, levels = geo_levels)
  
  # Create a highcharter plot with a dropdown for year selection
  years <- unique(summarized_data$year)
  
  # Initialize highchart
  hchart <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Employment Levels by Gender and Country (Working Age: 15-64)") %>%
    hc_subtitle(text = "Interactive Year Selection | Including EA20 and EU27_2020") %>%
    hc_xAxis(
      categories = levels(summarized_data$geo),
      title = list(text = "Country"),
      labels = list(
        rotation = 45,
        style = list(fontSize = "8px")
      )
    ) %>%
    hc_yAxis(
      title = list(text = "Total Employment"),
      labels = list(format = "{value:,}")
    ) %>%
    hc_plotOptions(column = list(grouping = TRUE)) %>%
    hc_tooltip(shared = TRUE, pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br>")
  
  # Add series for each year
  for (yr in years) {
    year_data <- summarized_data %>% filter(year == yr)
    
    hchart <- hchart %>%
      hc_add_series(
        data = year_data %>% filter(sex == "M") %>% arrange(geo) %>% pull(total_employment),
        name = paste0("Male - ", yr),
        color = "#003399",
        visible = ifelse(yr == min(years), TRUE, FALSE)
      ) %>%
      hc_add_series(
        data = year_data %>% filter(sex == "F") %>% arrange(geo) %>% pull(total_employment),
        name = paste0("Female - ", yr),
        color = "#FFCC00",
        visible = ifelse(yr == min(years), TRUE, FALSE)
      )
  }
  
  # Add year selection dropdown and exporting options with a toggleable legend
  hchart <- hchart %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE) %>%
    hc_chart(events = list(
      load = JS("
      function() {
        var chart = this;
        var legendVisible = true;
        var button = chart.renderer.button('Toggle Legend', 10, 10)
          .on('click', function() {
            legendVisible = !legendVisible;
            chart.update({
              legend: { enabled: legendVisible },
              xAxis: {
                labels: {
                  style: {
                    fontSize: '8px'
                  },
                  rotation: 45
                }
              }
            });
            // Adjust the chart margins dynamically
            if (!legendVisible) {
              chart.update({
                chart: { marginRight: 50 }
              });
            } else {
              chart.update({
                chart: { marginRight: 150 }
              });
            }
          })
          .add();
      }")
    ))
  
  # Display the chart
  hchart
}






