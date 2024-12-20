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

data <- get_eurostat("lfsa_egan") # 496671 observqtions, 8 variables
head(data)
str(data)

## Checking the unique values of the variables:
unique(data$freq) 
# "A" ---> Annual

unique(data$sex)
# "F" "M" "T" ---> (Female, Male, Total)

unique(data$unit)
# "THS_PER" ---> Thousand persons

unique(data$age)
# "Y15-19" "Y15-24" "Y15-39" "Y15-59" "Y15-64" "Y15-74" "Y20-24" "Y20-64" "Y25-29" "Y25-49" "Y25-54" "Y25-59" "Y25-64" "Y25-74"
# "Y30-34" "Y35-39" "Y40-44" "Y40-59" "Y40-64" "Y45-49" "Y50-54" "Y50-59" "Y50-64" "Y50-74" "Y55-59" "Y55-64" "Y60-64" "Y65-69"
# "Y65-74" "Y70-74" "Y_GE15" "Y_GE25" "Y_GE50" "Y_GE65" "Y_GE75"

unique(data$citizen) # Country of citizienship
# "EU27_2020_FOR" ---> EU27 countries (from 2020) except reporting country
# "NEU27_2020_FOR" ---> Non-EU27 countries (from 2020) nor reporting country
# "FOR" ---> Foreign Country
# "NAT" ---> Reporting Country
# "STLS" ---> Stateless       
# "NRP" ---> No response
# "TOTAL" ---> Total

unique(data$geo) # Geopolitical entity (reporting)
# "AT" "BE" "CH" "CY" "CZ" "DE" "DK" "EE" "EL"       
# "ES" "FI" "FR" "HU" "IE" "IS" "IT" "LU" "ME"       
# "MT" "NL" "NO" "PT" "RS" "SE" "SI" "SK" "UK" "BG"       
# "HR" "LT" "LV" "MK" "PL" "RO" "BA" "TR" "EU27_2020" "EA20"
# "EU27_2020" ---> European Union - 27 countries (from 2020)
# "EA20" ---> Euro area – 20 countries (from 2023)

unique(data$TIME_PERIOD)
# "1995-01-01" "1996-01-01" "1997-01-01" "1998-01-01" "1999-01-01" "2000-01-01" "2001-01-01" "2002-01-01" "2003-01-01"
# "2004-01-01" "2005-01-01" "2006-01-01" "2007-01-01" "2008-01-01" "2009-01-01" "2010-01-01" "2011-01-01" "2012-01-01"
# "2013-01-01" "2014-01-01" "2015-01-01" "2016-01-01" "2017-01-01" "2018-01-01" "2019-01-01" "2020-01-01" "2021-01-01"
# "2022-01-01" "2023-01-01"


#---------- sex ---------------#
{
  # Prepare the dataset (remove missing values and multiply by 1000 for actual values)
  data_clean <- data %>%
    filter(!is.na(values)) %>%
    mutate(values = values)  # Use original values for actual amounts
  
  # Aggregate data by sex (summarize total values per sex)
  sex_dist <- data_clean %>%
    group_by(sex) %>%
    summarise(total_value = sum(values, na.rm = TRUE))
  
  # Convert 'sex' to a factor with specific levels to avoid using numbers (0, 1, 2)
  sex_dist$sex <- factor(sex_dist$sex, levels = c("M", "F", "T"))
  
  # Create a highcharter pie chart (cake plot) for sex distribution with proper labels
  sex_plot <- highchart() %>%
    hc_add_series(sex_dist, type = "pie", hcaes(name = sex, y = total_value, color = c("#003399", "#FFCC00", "#404040")), 
                  name = "Distribution by Sex") %>%
    hc_title(text = "Distribution of Values by Sex") %>%
    hc_tooltip(pointFormat = "{point.name}: {point.percentage:.1f}%")  # Show percentage in tooltip
  
  # Display the plot
  sex_plot
}
#----------- age --------------#
{
  # Prepare the dataset (remove missing values and use original values for actual amounts)
  data_clean <- data %>%
    filter(!is.na(values)) %>%
    mutate(values = values)  # Use original values for actual amounts
  
  # Aggregate data by age (summarize total values per age group)
  age_dist <- data_clean %>%
    group_by(age) %>%
    summarise(total_value = sum(values, na.rm = TRUE))
  
  # Create a highcharter bar chart for age distribution with proper labels
  age_plot <- highchart() %>%
    hc_chart(zoomType = "xy") %>%  # Enable zooming along the x-axis
    hc_add_series(age_dist, type = "column", hcaes(x = age, y = total_value), 
                  name = "Distribution by Age", color = "#003399") %>%
    hc_title(text = "Distribution of Values by Age") %>%
    hc_xAxis(title = list(text = "Age Group"), categories = age_dist$age) %>%
    hc_yAxis(title = list(text = "Total Value (Thousands)"))
  
  # Display the plot
  age_plot
}
#----------- geo --------------#
{
  # Prepare the dataset (remove missing values and use original values for actual amounts)
  data_clean <- data %>%
    filter(!is.na(values)) %>%
    mutate(values = values)  # Use original values for actual amounts
  
  # Aggregate data by citizen (summarize total values per citizenship group)
  geo_dist <- data_clean %>%
    group_by(geo) %>%
    summarise(total_value = sum(values, na.rm = TRUE)) %>%
    arrange(desc(total_value))  # Sort by total_value in descending order
  
  # Create a highcharter horizontal bar chart for geo distribution with proper labels
  geo_plot <- highchart() %>%
    hc_chart(zoomType = "xy") %>%  # Enable zooming along the x-axis
    hc_add_series(geo_dist, type = "bar", hcaes(x = geo, y = total_value), 
                  name = "Distribution by Geopolitical Entity", color = "#FFCC00") %>%
    hc_title(text = "Distribution of Values by Geopolitical Entity") %>%
    hc_xAxis(title = list(text = "Geo Category"), categories = geo_dist$geo) %>%
    hc_yAxis(title = list(text = "Total Value (Thousands)"))
  
  # Display the plot
  geo_plot
}
#---------- citizen -----------#
{
  # Prepare the dataset (remove missing values and use original values for actual amounts)
  data_clean <- data %>%
    filter(!is.na(values)) %>%
    mutate(values = values)  # Use original values for actual amounts
  
  # Aggregate data by age (summarize total values per age group)
  citizen_dist <- data_clean %>%
    group_by(citizen) %>%
    summarise(total_value = sum(values, na.rm = TRUE))
  
  # Create a highcharter bar chart for citizen distribution with proper labels
  citizen_plot <- highchart() %>%
    hc_chart(zoomType = "xy") %>%  # Enable zooming along the x-axis
    hc_add_series(citizen_dist, type = "column", hcaes(x = citizen, y = total_value), 
                  name = "Distribution by Citizen Group", color = "#003399") %>%
    hc_title(text = "Distribution of Values by Citizen Group") %>%
    hc_xAxis(title = list(text = "Age Group"), categories = citizen_dist$citizen) %>%
    hc_yAxis(title = list(text = "Total Value (Thousands)"))
  
  # Display the plot
  citizen_plot
}
#---------TIME_PERIOD ---------#
{
  # Prepare the dataset (remove missing values and use original values for actual amounts)
  data_clean <- data %>%
    filter(!is.na(values)) %>%
    mutate(values = values, year = year(TIME_PERIOD))  # Extract year from TIME_PERIOD
  
  # Aggregate data by year (summarize total values per year)
  time_dist <- data_clean %>%
    group_by(year) %>%
    summarise(total_value = sum(values, na.rm = TRUE))
  
  # Create a highcharter bar chart for the distribution by year
  time_plot <- highchart() %>%
    hc_chart(zoomType = "xy") %>%  # Enable zooming along the x-axis
    hc_add_series(time_dist, type = "column", hcaes(x = year, y = total_value), 
                  name = "Distribution by Year", color = "#003399") %>%
    hc_title(text = "Distribution of Values by Year") %>%
    hc_xAxis(title = list(text = "Year"), categories = time_dist$year) %>%
    hc_yAxis(title = list(text = "Total Value (Thousands)"))
  
  # Display the plot
  time_plot
}
#---------Missing values ------#
{
  # Calculate the number of missing and non-missing values in the 'values' column
  missing_count <- sum(is.na(data$values))
  non_missing_count <- sum(!is.na(data$values))
  
  # Create a data frame for the pie chart
  missing_data <- data.frame(
    category = c("Missing Values", "Non-Missing Values"),
    count = c(missing_count, non_missing_count)
  )
  
  # Create the highcharter pie chart for missing vs non-missing values
  missing_pie_plot <- highchart() %>%
    hc_add_series(missing_data, type = "pie", hcaes(name = category, y = count, color = c("#FFCC00", "#003399")), 
                  name = "Missing vs Non-Missing", 
    ) %>%
    hc_title(text = "Missing vs Non-Missing Values") %>%
    hc_tooltip(pointFormat = "{point.name}: {point.percentage:.1f}%")
  
  # Display the plot
  missing_pie_plot
  
}


# sex_plot
# age_plot
# geo_plot
# citizen_plot
# time_plot
# missing_pie_plot

hw_grid(
  missing_pie_plot, time_plot, sex_plot, 
  age_plot, geo_plot, citizen_plot,
  ncol = 3,
  rowheight = NULL,
  add_htmlgrid_css = TRUE,
  browsable = TRUE
)
