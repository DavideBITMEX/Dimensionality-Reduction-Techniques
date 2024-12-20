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

# Trends by Citizen Group
##
plot_employment_trends <- function(data, 
                                   age_filter = "Y15-64", 
                                   sex_filter = "T", 
                                   citizen_filter = unique(data$citizen),
                                   tickInterval = 50000000,
                                   include_geo = c("EU27_2020", "CH", "IS", "ME", "NO", "RS", "UK", "MK", "BA", "TR"),
                                   plot_title = "Employment Trends by Citizenship Group (All European Countries)") {
  # Filter data for the specified age, sex, and only include specified countries
  working_age_data <- data %>% 
    filter(
      age == age_filter,
      sex == sex_filter,
      geo %in% include_geo
    ) %>%
    mutate(
      year = year(TIME_PERIOD),
      employment_real = values * 1000  # Convert employment values to actual numbers
    )
  
  # Group employment by year and citizenship
  time_trend <- working_age_data %>%
    group_by(TIME_PERIOD, citizen) %>%
    summarise(total_employment = sum(employment_real, na.rm = TRUE)) %>%
    ungroup()
  
  # Create the Highcharter plot
  hchart <- highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = plot_title) %>%
    hc_subtitle(text = paste("Working Age:", age_filter, "| Females and Males together")) %>%
    hc_xAxis(
      type = "datetime",
      title = list(text = "Year"),
      labels = list(format = "{value:%Y}")
    ) %>%
    hc_yAxis(
      title = list(text = "Total Employment"),
      labels = list(format = "{value:,.0f}"),  # Formats large numbers with commas
      tickInterval = tickInterval
    ) %>%
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      pointFormat = "<b>{series.name}</b> - <b>{point.y:,.0f}</b><br>"
    ) %>%
    hc_colors(c(
      "#003399",  # EU27_2020_FOR (Dark Blue)
      "#DAA520",  # FOR (Goldenrod)
      "#228B22",  # NAT (Forest Green)
      "#00CED1",  # NEU27_2020_FOR (Dark Turquoise)
      "#1E90FF",  # NRP (Dodger Blue)
      "#9370DB",  # STLS (Medium Purple)
      "#FF1493"   # TOTAL (Deep Pink)
    )) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  
  # Add series for each citizenship group
  citizenship_groups <- unique(time_trend$citizen)
  for (citizen in citizenship_groups) {
    citizen_data <- time_trend %>% filter(citizen == !!citizen)
    hchart <- hchart %>%
      hc_add_series(
        data = list_parse2(data.frame(
          x = as.numeric(as.POSIXct(citizen_data$TIME_PERIOD)) * 1000,
          y = citizen_data$total_employment
        )),
        name = citizen
      )
  }
  
  # Define event annotations (vertical dashed lines)
  event_annotations <- data.frame(
    date = as.Date(c("2001-01-01", "2004-01-01", "2007-01-01", "2008-01-01", "2020-01-01"))
  )
  
  plot_lines <- lapply(1:nrow(event_annotations), function(i) {
    list(
      color = "#084594",          # Dark blue for the line
      width = 2,                  # Line thickness
      value = as.numeric(as.POSIXct(event_annotations$date[i])) * 1000,
      dashStyle = "Dash",         # Dashed line style
      zIndex = 3,                 # Ensure lines are above the plot
      label = list(
        text = "",                # No static text
        style = list(fontSize = "0px")  # Hide the label
      )
    )
  })
  
  # Add the plot lines to the Highcharter plot
  hchart <- hchart %>%
    hc_xAxis(plotLines = plot_lines) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
  
  return(hchart)
}

plot_employment_trends(data)

##
plot_employment_trends(data, include_geo = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"), age_filter = "Y15-64", citizen_filter = unique(data$citizen), plot_title = "Employment Trends by Citizenship Group (EU27_2020)")

##
plot_employment_trends(data, include_geo = c("CH", "IS", "ME", "NO", "RS", "UK", "MK", "BA", "TR"), age_filter = "Y15-64", citizen_filter = unique(data$citizen), plot_title = "Employment Trends by Citizenship Group (non EU27)")


# Trend by Country (EU State Members)
##
{
eu27_countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL",
                    "ES","FI","FR","HR","HU","IE","IT","LT","LU",
                    "LV","MT","NL","PL","PT","RO","SE","SI","SK")

# Filter dataset for desired age, sex, and EU27 countries
temporary_data <- data %>%
  filter(
    age == "Y15-64",
    sex == "T",
    geo %in% eu27_countries
  ) %>%
  mutate(
    year = year(TIME_PERIOD),
    employment_real = values * 1000
  )

# Keep only FOR, NAT, STLS, NRP and aggregate them into MY_TOTAL
eu_data <- temporary_data %>%
  filter(citizen %in% c("FOR", "NRP", "STLS", "NAT")) %>%
  group_by(year, geo) %>%
  summarise(MY_TOTAL = sum(employment_real, na.rm = TRUE), .groups = "drop") %>%
  # Exclude EU27_2020 and EA20
  filter(!geo %in% c("EU27_2020", "EA20"))

# Pivot data so each geo is a column, one row per year
eu_data_wide <- eu_data %>%
  pivot_wider(
    names_from = geo, 
    values_from = MY_TOTAL, 
    values_fill = 0
  )

# Calculate total contribution per country across all years
country_totals <- colSums(eu_data_wide[,-1]) # exclude the 'year' column
# Sort countries by total contribution (descending)
sorted_countries <- names(sort(country_totals, decreasing = TRUE))

# Reorder columns according to sorted countries
eu_data_wide <- eu_data_wide[, c("year", sorted_countries)]

# Prepare the series list as [x,y] pairs for each country
series_list <- lapply(sorted_countries, function(country) {
  data_pairs <- Map(function(x_val, y_val) list(x = x_val, y = y_val), eu_data_wide$year, eu_data_wide[[country]])
  list(
    name = country,
    data = data_pairs
  )
})

# Create the stacked area chart
hchart <- highchart() %>%
  hc_chart(type = "area") %>%
  hc_title(text = "EU27_2020 Countries Total Contribution Over Time") %>%
  hc_xAxis(
    title = list(text = "Year"),
    labels = list(format = "{value}")
  ) %>%
  hc_yAxis(
    title = list(text = "Total Employment"),
    labels = list(format = "{value:,.0f}")
  ) %>%
  hc_plotOptions(
    area = list(stacking = "normal", marker = list(enabled = FALSE))
  ) %>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = "<b>{series.name}:</b> {point.y:,.0f}<br>",
    headerFormat = "<b>Year: {point.x}</b><br/>"
  ) %>%
  hc_add_series_list(series_list) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

hchart
}

# Trends by Age
##
plot_employment_trends(data, include_geo = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"), age_filter = "Y15-24", citizen_filter = unique(data$citizen), tickInterval = 5000000, plot_title = "Employment Trends for Youths (EU27_2020)")

##
plot_employment_trends(data, include_geo = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"), age_filter = "Y25-54", citizen_filter = unique(data$citizen), tickInterval = 20000000, plot_title = "Employment Trends for Prime Working Age (EU27_2020)")

##
plot_employment_trends(data, include_geo = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"), age_filter = "Y55-64", citizen_filter = unique(data$citizen), tickInterval = 10000000, plot_title = "Employment Trends for Older Working Age (EU27_2020)")










