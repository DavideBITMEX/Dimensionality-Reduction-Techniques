# EU Employment Analysis (EUROSTAT Data)

# Load the necessary libraries
library(tidyverse)     # For data manipulation and visualization
library(eurostat)      # For downloading Eurostat data
library(FactoMineR)    # For performing PCA
library(factoextra)    # For visualizing PCA results
#library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(tidyverse)
library(plotly)
library(scales)  # For formatting the y-axis
library(patchwork)  # For combining multiple plots
library(gridExtra)  # For plotting more graphs in the same window
library(highcharter)




# Download the 'lfsa_egan' dataset from Eurostat
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

#-------------------------------------------#
#--- Employment levels (F and M, Y15-64) ---#
#---- All countries grouped by year --------#
#------- Excluding geo aggregates ----------#
#-------------------------------------------#
{
  # Filter the data for working age (Y15-64), Males (M), Females (F), and exclude EU27_2020 and EA20
  filtered_data <- data %>%
    filter(
      age == "Y15-64",
      sex %in% c("M", "F"),
      !geo %in% c("EU27_2020", "EA20")
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
    hc_title(text = "Employment Levels by Gender for the Working Age Population (15-64) Over Time") %>%
    hc_subtitle(text = "All countries (excluding EU aggregates EU27_2020 - EA20)") %>%
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
      color = "#003399"
    ) %>%
    hc_add_series(
      data = summarized_data %>% filter(sex == "F") %>% pull(total_employment),
      name = "Female",
      color = "#FFCC00"
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE)  # Enable export options
  
  # Display the chart
  hchart
}

#-------------------------------------------#
#--- Employment levels (F and M, Y15-64) ---#
#---- Per citizien grouped per country -----#
#------- Including just citiz Total --------#
#-------------------------------------------#
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
  
  # Verify the factor levels
  print(levels(summarized_data$geo))
  
  # Create a highcharter plot with a dropdown for year selection
  years <- unique(summarized_data$year)
  
  # Initialize highchart
  hchart <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Employment Levels by Gender and Country (Working Age: 15-64) ") %>%
    hc_subtitle(text = "Interactive Year Selection | Including EA20 and EU27_2020") %>%
    hc_xAxis(categories = levels(summarized_data$geo),
             title = list(text = "Country"),
             labels = list(rotation = 45)) %>%
    hc_yAxis(title = list(text = "Total Employment"),
             labels = list(format = "{value:,}")) %>%
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
  
  # Add year selection dropdown
  hchart <- hchart %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
  
  # Display the chart
  hchart
}

#-------------------------------------------#
#------------ Trends over time -------------#
#-------------------------------------------#
{
  # Filter data for working-age population (15-64) and 'Total' for sex
  working_age_data <- data %>%
    filter(age == "Y15-64", sex == "T") %>%
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
    hc_title(text = "Employment Trends by Citizenship Group (1995-2023)") %>%
    hc_subtitle(text = "Working Age: 15-64") %>%
    hc_xAxis(
      type = "datetime",
      title = list(text = "Year"),
      labels = list(format = "{value:%Y}")
    ) %>%
    hc_yAxis(
      title = list(text = "Total Employment"),
      labels = list(format = "{value:,}")
    ) %>%
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      pointFormat = "<b>Citizen Group:</b> {series.name}<br><b>Total Employment:</b> {point.y:,.0f}<br>"
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
  
  # Define the event annotations
  event_annotations <- data.frame(
    date = as.Date(c("2001-01-01", "2004-01-01", "2007-01-01", "2008-01-01", "2020-01-01"))
  )
  
  # Add vertical dashed lines for each event annotation with hover tooltips
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
    hc_xAxis(plotLines = plot_lines)
  
  # Enable exporting and display the plot
  hchart <- hchart %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE)
  
  # Display the chart
  hchart

# Overall Employment Growth:
#   Total Employment (Pink Line) shows a sharp increase between 2004 and 2007, stabilizing afterward. 
#   This increase coincides with the 2004 EU Enlargement and 2007 accession of Bulgaria and Romania.

# Native Employment (NAT - Green Line):
#   Employment of native citizens (NAT) follows a similar pattern to the total employment trend. 
#   The growth stabilizes post-2007 and experiences a slight dip during the COVID-19 pandemic in 2020.

# EU27_2020_FOR (Dark Blue Line):
#   Employment of EU citizens working in other EU countries (EU27_2020_FOR) shows a relatively stable trend with moderate 
#   growth over the period. There is a noticeable decline around 2008, likely related to the global financial crisis.

# Foreign (FOR - Goldenrod Line):
#   Employment of non-EU foreign citizens (FOR) exhibits a gradual but consistent increase, indicating the 
#   growing role of foreign labor in the EU economy.

# NRP (Light Blue Line):
#   No Response (NRP) category fluctuates significantly, suggesting inconsistencies in reporting or data collection.

# Impact of Events:
#   2004: EU Enlargement saw a significant rise in employment levels, particularly for native citizens and total employment.
#   2007: Bulgaria & Romania Join the EU led to a continued increase in employment levels.
#   2020: COVID-19 Pandemic caused a slight dip in employment, reflecting the economic disruption during this period.
#         The sharp fluctuations suggest potential inconsistencies or anomalies in the data collection for this category.

# The employment trends highlight the impact of EU expansions and major global events on labor markets.
# Native citizens (NAT) continue to represent the bulk of employment, while foreign labor (FOR) and 
# cross-border employment (EU27_2020_FOR) play significant, though smaller, roles.
# The COVID-19 pandemic caused a temporary disruption but did not significantly alter the long-term trend.

}

#-------------------------------------------#
#--- Comparison of Employment by Country ---#
#-------------------------------------------#
{
# Which countries have the highest and lowest employment rates?
# How does employment vary between different countries for specific citizenship categories?

# Implementation: Heatmap for Selected Years, Working Population (Y15-64), Male and Females (T)
# I decided to create heatmaps for the following years:
  # 2000: Represents the early 2000s before major EU enlargements.
  # 2010: Captures employment trends post-2008 financial crisis.
  # 2023: Reflects the latest available data, including post-COVID-19 recovery.
  # I'll proceed with this approach and generate heatmaps for these years.

# Filter data for selected years and only 'Total' for sex and working-age population (15-64)
selected_years <- c("2000-01-01", "2010-01-01", "2023-01-01")
filtered_data <- data %>%
  filter(TIME_PERIOD %in% as.Date(selected_years), sex == "T", age == "Y15-64")

# Group employment by country, citizenship, and year
country_citizen_summary <- filtered_data %>%
  group_by(geo, citizen, TIME_PERIOD) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))

# Function to create a plotly heatmap for a specific year
create_plotly_heatmap <- function(year, showlegend = TRUE) {
  df <- country_citizen_summary %>% filter(TIME_PERIOD == as.Date(year))
  
  plot_ly(
    data = df,
    x = ~geo,
    y = ~citizen,
    z = ~total_employment,
    type = "heatmap",
    colors = colorRamp(c("#f0f9ff", "#084594")),
    colorbar = list(
      title = "<b>Employment (Thousands)</b>",
      tickfont = list(size = 12),
      titlefont = list(size = 14, family = "Arial")
    ),
    showscale = showlegend,   # Control legend display
    zmin = 0, zmax = 200000   # Set consistent color scale limits
  ) %>%
    layout(
      title = list(
        #text = paste("<b>Employment by Country and Citizenship -", format(as.Date(year), "%Y"), "</b>"),
        font = list(size = 18, family = "Arial"),
        x = 0.5,  # Center the title
        xanchor = "center"
      ),
      xaxis = list(
        title = "<b>Country</b>",
        tickangle = 45,
        tickfont = list(size = 10),
        titlefont = list(size = 14, family = "Arial")
      ),
      yaxis = list(
        title = "<b>Citizenship Category</b>",
        tickfont = list(size = 10),
        titlefont = list(size = 14, family = "Arial")
      ),
      margin = list(t = 60, b = 60)  # Add padding to top and bottom margins
    )
}

# Create plotly heatmaps for each selected year
interactive_heatmap_2000 <- create_plotly_heatmap("2000-01-01", showlegend = TRUE)
interactive_heatmap_2010 <- create_plotly_heatmap("2010-01-01", showlegend = FALSE)
interactive_heatmap_2023 <- create_plotly_heatmap("2023-01-01", showlegend = FALSE)

# Combine the interactive heatmaps vertically with a single legend
combined_interactive_heatmaps <- subplot(
  interactive_heatmap_2000,
  interactive_heatmap_2010,
  interactive_heatmap_2023,
  nrows = 3,
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE
) %>%
  layout(
    title = "Employment Trends by Citizenship Category and Country (2000-2010-2023)",
    margin = list(l = 100, r = 50, t = 80, b = 100),  # Adjust margins for better spacing
    titlefont = list(size = 20, family = "Arial", color = "black")
  )

# Display the combined interactive heatmaps
combined_interactive_heatmaps

# General Trends (2000, 2010, 2023)
# Employment Levels:
#   Employment levels vary significantly across countries and citizenship categories.
# The color intensity reflects the number of employed persons (darker blue = higher employment).

# 2000:
#   Employment for nationals (NAT) dominates in most countries.
# Lower employment levels for foreign-born categories (EU27_2020_FOR and NEU27_2020_FOR).

# 2010:
#   Noticeable increase in employment for foreign-born categories, reflecting the impact of EU enlargement (2004 and 2007).
#   Some countries show declines, possibly due to the 2008 financial crisis.

# 2023:
#   Recovery in employment post-COVID-19 pandemic.
#   Continued presence of foreign-born employment, especially in countries like Germany (DE), France (FR), and Spain (ES).

#Key Insights:
# Foreign Employment Growth:
#   Employment for foreign-born categories has increased over time, indicating greater workforce integration and migration trends.
# Country-Specific Patterns:
#   Germany (DE) and France (FR) consistently show high employment levels across all years.
#   Southern European countries like Spain (ES) show fluctuations due to economic events.
# Policy Implications:
#   Insights from these trends can help shape employment policies and labor market integration programs within the EU.
}

#-------------------------------------------#
#--- Comparison of Employment by Country ---#
#--- Without EU27_2020 and EA20 ------------#
#-------------------------------------------#
{
  # Which countries have the highest and lowest employment rates?
  # How does employment vary between different countries for specific citizenship categories?
  
  # Implementation: Heatmap for Selected Years, Working Population (Y15-64), Male and Females (T)
  # I decided to create heatmaps for the following years:
  # 2000: Represents the early 2000s before major EU enlargements.
  # 2010: Captures employment trends post-2008 financial crisis.
  # 2023: Reflects the latest available data, including post-COVID-19 recovery.
  # I'll proceed with this approach and generate heatmaps for these years.
  
  # Filter data for selected years and only 'Total' for sex and working-age population (15-64)
  selected_years <- c("2000-01-01", "2010-01-01", "2023-01-01")
  filtered_data <- data %>%
    filter(
      TIME_PERIOD %in% as.Date(selected_years),
      sex == "T",
      age == "Y15-64",
      !geo %in% c("EU27_2020", "EA20")  # Exclude "EU27_2020" and "EA20"
    )
  
  
  # Group employment by country, citizenship, and year
  country_citizen_summary <- filtered_data %>%
    group_by(geo, citizen, TIME_PERIOD) %>%
    summarise(total_employment = sum(values, na.rm = TRUE))
  
  # Function to create a plotly heatmap for a specific year
  create_plotly_heatmap <- function(year, showlegend = TRUE) {
    df <- country_citizen_summary %>% filter(TIME_PERIOD == as.Date(year))
    
    plot_ly(
      data = df,
      x = ~geo,
      y = ~citizen,
      z = ~total_employment,
      type = "heatmap",
      colors = colorRamp(c("#f0f9ff", "#084594")),
      colorbar = list(
        title = "<b>Employment (Thousands)</b>",
        tickfont = list(size = 12),
        titlefont = list(size = 14, family = "Arial")
      ),
      showscale = showlegend,   # Control legend display
      zmin = 0, zmax = 50000   # Set consistent color scale limits
    ) %>%
      layout(
        title = list(
          #text = paste("<b>Employment by Country and Citizenship -", format(as.Date(year), "%Y"), "</b>"),
          font = list(size = 18, family = "Arial"),
          x = 0.5,  # Center the title
          xanchor = "center"
        ),
        xaxis = list(
          title = "<b>Country</b>",
          tickangle = 45,
          tickfont = list(size = 10),
          titlefont = list(size = 14, family = "Arial")
        ),
        yaxis = list(
          title = "<b>Citizenship Category</b>",
          tickfont = list(size = 10),
          titlefont = list(size = 14, family = "Arial")
        ),
        margin = list(t = 60, b = 60)  # Add padding to top and bottom margins
      )
  }
  
  # Create plotly heatmaps for each selected year
  interactive_heatmap_2000 <- create_plotly_heatmap("2000-01-01", showlegend = FALSE)
  interactive_heatmap_2010 <- create_plotly_heatmap("2010-01-01", showlegend = TRUE)
  interactive_heatmap_2023 <- create_plotly_heatmap("2023-01-01", showlegend = FALSE)
  
  # Combine the interactive heatmaps vertically
  combined_interactive_heatmaps <- subplot(
    interactive_heatmap_2000,
    interactive_heatmap_2010,
    interactive_heatmap_2023,
    nrows = 3,
    shareX = TRUE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = FALSE
  ) %>%
    layout(
      annotations = list(
        list(
          text = "<b>Year: 2000</b>",
          x = 0.5,
          y = 1.03,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, family = "Arial")
        ),
        list(
          text = "<b>Year: 2010</b>",
          x = 0.5,
          y = 0.68,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, family = "Arial")
        ),
        list(
          text = "<b>Year: 2023</b>",
          x = 0.5,
          y = 0.31,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, family = "Arial")
        )
      ),
      title = "<b>Employment Trends by Citizenship Category and Country (2000-2010-2023)</b>",
      margin = list(l = 100, r = 50, t = 80, b = 100),
      titlefont = list(size = 20, family = "Arial", color = "black")
    )
  
  # Display the combined interactive heatmaps
  combined_interactive_heatmaps

  
  # General Trends (2000, 2010, 2023)
  # Employment Levels:
  #   Employment levels vary significantly across countries and citizenship categories.
  # The color intensity reflects the number of employed persons (darker blue = higher employment).
  
  # 2000:
  #   Employment for nationals (NAT) dominates in most countries.
  # Lower employment levels for foreign-born categories (EU27_2020_FOR and NEU27_2020_FOR).
  
  # 2010:
  #   Noticeable increase in employment for foreign-born categories, reflecting the impact of EU enlargement (2004 and 2007).
  #   Some countries show declines, possibly due to the 2008 financial crisis.
  
  # 2023:
  #   Recovery in employment post-COVID-19 pandemic.
  #   Continued presence of foreign-born employment, especially in countries like Germany (DE), France (FR), and Spain (ES).
  
  #Key Insights:
  # Foreign Employment Growth:
  #   Employment for foreign-born categories has increased over time, indicating greater workforce integration and migration trends.
  # Country-Specific Patterns:
  #   Germany (DE) and France (FR) consistently show high employment levels across all years.
  #   Southern European countries like Spain (ES) show fluctuations due to economic events.
  # Policy Implications:
  #   Insights from these trends can help shape employment policies and labor market integration programs within the EU.
}

#-------------------------------------------------------------#
#- Gender-Based Employment Trends by Citizenship and Country -#
#-------------------------------------------------------------#
{
# Filter data for selected years, working-age population (15-64), and by sex
gender_data <- data %>%
  filter(
    TIME_PERIOD %in% as.Date(selected_years),
    age == "Y15-64",
    sex %in% c("F", "M", "T")  # Female, Male, Total
  )

# Group employment by year, country, citizenship, and sex
gender_summary <- gender_data %>%
  group_by(TIME_PERIOD, geo, citizen, sex) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))


# Function to create a plotly heatmap for gender-based employment
create_gender_heatmap <- function(year, showlegend = TRUE) {
  df <- gender_summary %>% filter(TIME_PERIOD == as.Date(year))
  
  plot_ly(
    data = df,
    x = ~geo,
    y = ~interaction(citizen, sex, sep = " - "),  # Combine citizenship and sex
    z = ~total_employment,
    type = "heatmap",
    colors = colorRamp(c("#f0f9ff", "#084594")),
    colorbar = list(
      title = "<b>Employment (Thousands)</b>",
      tickfont = list(size = 12),
      titlefont = list(size = 14, family = "Arial")
    ),
    showscale = showlegend,
    zmin = 0, zmax = 200000  # Set consistent color scale limits
  ) %>%
    layout(
      title = list(
        text = paste("<b>Gender-Based Employment by Country and Citizenship -", format(as.Date(year), "%Y"), "</b>"),
        font = list(size = 18, family = "Arial"),
        x = 0.5,  # Center the title
        xanchor = "center"
      ),
      xaxis = list(
        title = "<b>Country</b>",
        tickangle = 45,
        tickfont = list(size = 10),
        titlefont = list(size = 14, family = "Arial")
      ),
      yaxis = list(
        title = "<b>Citizenship - Gender</b>",
        tickfont = list(size = 10),
        titlefont = list(size = 14, family = "Arial")
      ),
      margin = list(t = 60, b = 60)  # Add padding to top and bottom margins
    )
}

# Create interactive gender heatmaps for selected years
gender_heatmap_2000 <- create_gender_heatmap("2000-01-01", showlegend = TRUE)
gender_heatmap_2010 <- create_gender_heatmap("2010-01-01", showlegend = FALSE)
gender_heatmap_2023 <- create_gender_heatmap("2023-01-01", showlegend = FALSE)

# Combine the interactive heatmaps vertically with a single legend
combined_gender_heatmaps <- subplot(
  gender_heatmap_2000,
  gender_heatmap_2010,
  gender_heatmap_2023,
  nrows = 3,
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE
) %>%
  layout(
    title = "Gender-Based Employment Trends by Citizenship and Country (2000-2010-2023)",
    margin = list(l = 100, r = 50, t = 80, b = 100),
    titlefont = list(size = 20, family = "Arial", color = "black")
  )


# Display the combined interactive heatmaps
combined_gender_heatmaps
}
#-------------------------------------------#




#-------------------------------------------#
#- Employment Distribution by citizienship -#
#-------------------------------------------#
# What is the distribution of employment between nationals and foreign-born workers?
# Are there significant differences in employment rates between EU-born and non-EU-born populations?

# Summarize employment by citizenship category
citizen_summary <- data %>%
  group_by(citizen) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))

# Plot a bar chart
ggplot(citizen_summary, aes(x = citizen, y = total_employment, fill = citizen)) +
  geom_bar(stat = "identity") +
  labs(title = "Employment Distribution by Citizenship",
       x = "Citizenship Category",
       y = "Total Employment (Thousands)") +
  theme_minimal()

#-------------------------------------------#
#----- Gender differences in employment ----#
#-------------------------------------------#

# Filter and group by sex and citizenship
gender_summary <- data %>%
  group_by(sex, citizen) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))

# Plot a grouped bar chart
ggplot(gender_summary, aes(x = citizen, y = total_employment, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment by Citizenship and Gender",
       x = "Citizenship Category",
       y = "Total Employment (Thousands)",
       fill = "Sex") +
  theme_minimal()

#-------------------------------------------#

# Aggiungi una colonna con valori negativi per i maschi
data_vis <- data %>%
  mutate(values_adj = ifelse(sex == "M", -values, values)) %>%
  mutate(sex_MF = ifelse(sex %in% c("M", "F"), sex, 0)) %>%
# filter(sex == "F" | sex == "M") %>%
  mutate(
    # Crea una colonna temporanea per il riordino: assegna un valore speciale per 'T'
    reorder_value = ifelse(sex == "T", Inf, as.numeric(values)),
    # Ordina geo in base alla nuova colonna temporanea
    geo = fct_reorder(geo, reorder_value, .fun = median, .desc = FALSE)
  )
# Plot con ggplot2
ggplot(data_vis, aes(x = geo, y = values_adj, fill = sex_MF)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(labels = abs) +  # Remove negative values on Y axe
  coord_flip() +  # Rotate plot 90 degrees
  labs(title = "Stacked Pyramid Plot per Genere",
       x = "Geopolitical Entity",
       y = "") +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "skyblue", "F" = "pink"))

# Non E' proprio quello che voglio. Ti ho incollato il mio codice attuale e ti dico quello che vorrei: data contiene $geo (chr), sex (chr che puo essere M, F o T) e values. Ho provato a creare sex_MF ma non ho ottenuto il risultato che speravo. Voglio creare questo stacked pyramid plot per genre dove si visualizzano solo i valori legati a M e F (non voglio i Totals(T)) ma che vengano ordinati in ordine descresente di T. Inoltre, nell'asse delle x non vorrei il conteggio ma vorrei i values 

# Supponiamo che 'data' sia il tuo dataset originale
data_vis <- data %>%
  # Crea una colonna per i valori negativi per i maschi
  mutate(values_adj = ifelse(sex == "M", -values, values)) %>%
  # Filtra per includere solo 'M' e 'F'
  filter(sex %in% c("M", "F")) 
  

data_2023 <- data_vis %>%
  filter(TIME_PERIOD >= as.Date("2023-01-01") & TIME_PERIOD <= as.Date("2023-12-31")) %>%
  filter(age == "Y20-64")

# voglio ordinare le oss
  
# Plot con ggplot2
ggplot(data_2023, aes(x = geo, y = values_adj, fill = sex)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(labels = abs) +  # Mostra i valori assoluti sull'asse Y
  coord_flip() +  # Ruota il grafico di 90 gradi
  labs(
    title = "Stacked Pyramid Plot per Genere",
    x = "Geopolitical Entity",
    y = "Values"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "skyblue", "F" = "pink"))
#-------------------------------------------#
#-------------------------------------------#
#-------------------------------------------#


# Filter data for:
# - Time period between 2021 and 2023
# - Age group 20-64 years (working-age population)
# - Both sexes (sex == "T")
# - Employment in thousands of persons (unit == "THS_PER")
data_filtered <- data %>%
  filter(TIME_PERIOD >= as.Date("2021-01-01") & TIME_PERIOD <= as.Date("2023-12-31"),
         age == "Y20-64", 
         sex == "T",
         unit == "THS_PER") %>%
  drop_na(values)  # Remove rows with missing values in 'values'

# Summarize employment by citizenship category
citizen_summary <- data_filtered %>%
  group_by(citizen) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))

# Plot a bar chart
ggplot(citizen_summary, aes(x = citizen, y = total_employment, fill = citizen)) +
  geom_bar(stat = "identity") +
  labs(title = "Employment Distribution by Citizenship",
       x = "Citizenship Category",
       y = "Total Employment (Thousands)") +
  theme_minimal()


# Group employment by year and citizenship
time_trend <- data_filtered %>%
  group_by(TIME_PERIOD, citizen) %>%
  summarise(total_employment = sum(values, na.rm = TRUE))

# Plot a line chart
ggplot(time_trend, aes(x = TIME_PERIOD, y = total_employment, color = citizen)) +
  geom_line(size = 1) +
  labs(title = "Employment Trends by Citizenship (2021-2023)",
       x = "Year",
       y = "Total Employment (Thousands)",
       color = "Citizenship") +
  theme_minimal()


# Transform the data:
# - Create a wide-format dataset with countries as rows and citizen categories as columns.
# - Fill missing values with 0.
pca_data <- data_filtered %>%
  pivot_wider(names_from = citizen, values_from = values, values_fill = 0)
