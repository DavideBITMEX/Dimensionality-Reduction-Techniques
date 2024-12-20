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

# Employment's comparison by Country and Citizienship Category
## Heatmaps Comparison - Before and After the EU Enlargements (2000 and 2007)
{
  # Filter data for selected years and only 'Total' for sex and working-age population (15-64)
  selected_years <- c("2000-01-01", "2007-01-01")
  filtered_data <- data %>%
    filter(
      TIME_PERIOD %in% as.Date(selected_years),
      sex == "T",
      age == "Y15-64",
      !citizen %in% c("EU27_2020_FOR", "NEU27_2020_FOR", "TOTAL"),
      !geo %in% c("EU27_2020", "EA20")  # Exclude "EU27_2020" and "EA20"
    )
  
  # Group employment by country, citizenship, and year
  country_citizen_summary <- filtered_data %>%
    group_by(geo, citizen, TIME_PERIOD) %>%
    summarise(total_employment = sum(values, na.rm = TRUE))
  
  # Function to create a plotly heatmap for a specific year
  create_plotly_heatmap <- function(year, showlegend = TRUE) {
    df <- country_citizen_summary %>% filter(TIME_PERIOD == as.Date(year))
    
    # Multiply total_employment by 1,000 to reflect real values
    df <- df %>% mutate(total_employment_real = total_employment * 1000)
    
    plot_ly(
      data = df,
      x = ~geo,
      y = ~citizen,
      z = ~total_employment_real,
      type = "heatmap",
      colors = colorRamp(c("#f0f9ff", "#003399")),
      colorbar = list(
        title = "<b>Employment</b>",
        tickfont = list(size = 12),
        titlefont = list(size = 14, family = "Arial")
      ),
      showscale = showlegend,   # Control legend display
      zmin = 0, zmax = 50000000,   # Set consistent color scale limits
      
      # Custom hover text
      hovertemplate = paste(
        "<b>Country:</b> %{x}<br>",
        "<b>Citizenship:</b> %{y}<br>",
        "<b>Total Employment:</b> %{z}<br>",
        "<extra></extra>"  # Removes default trace info
      )
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
  interactive_heatmap_2007 <- create_plotly_heatmap("2007-01-01", showlegend = FALSE)
  
  # Combine the interactive heatmaps vertically
  combined_interactive_heatmaps <- subplot(
    interactive_heatmap_2000,
    interactive_heatmap_2007,
    nrows = 2,
    shareX = TRUE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    layout(
      annotations = list(
        list(
          text = "<b>Year: 2000 (before EU enlargements)</b>",
          x = 0.5,
          y = 1.06,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, family = "Arial")
        ),
        list(
          text = "<b>Year: 2007 (after EU enlargements)</b>",
          x = 0.5,
          y = 0.50,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, family = "Arial")
        )
      ),
      title = "<b>Employment Trends by Citizenship Category and Country (2000 vs 2007)</b>",
      margin = list(l = 100, r = 50, t = 80, b = 100),
      titlefont = list(size = 20, family = "Arial", color = "black")
    )
  
  # Display the combined interactive heatmaps
  combined_interactive_heatmaps
}

## Heatmaps Comparison - After 2 Important Crisis (2010 and 2023)
{
  # Filter data for selected years and only 'Total' for sex and working-age population (15-64)
  selected_years <- c("2010-01-01", "2023-01-01")
  filtered_data <- data %>%
    filter(
      TIME_PERIOD %in% as.Date(selected_years),
      sex == "T",
      age == "Y15-64",
      !citizen %in% c("EU27_2020_FOR", "NEU27_2020_FOR", "TOTAL"),
      !geo %in% c("EU27_2020", "EA20")  # Exclude "EU27_2020" and "EA20"
    )
  
  # Group employment by country, citizenship, and year
  country_citizen_summary <- filtered_data %>%
    group_by(geo, citizen, TIME_PERIOD) %>%
    summarise(total_employment = sum(values, na.rm = TRUE))
  
  # Function to create a plotly heatmap for a specific year
  create_plotly_heatmap <- function(year, showlegend = TRUE) {
    df <- country_citizen_summary %>% filter(TIME_PERIOD == as.Date(year))
    
    # Multiply total_employment by 1,000 to reflect real values
    df <- df %>% mutate(total_employment_real = total_employment * 1000)
    
    plot_ly(
      data = df,
      x = ~geo,
      y = ~citizen,
      z = ~total_employment_real,
      type = "heatmap",
      colors = colorRamp(c("#f0f9ff", "#003399")),
      colorbar = list(
        title = "<b>Employment</b>",
        tickfont = list(size = 12),
        titlefont = list(size = 14, family = "Arial")
      ),
      showscale = showlegend,   # Control legend display
      zmin = 0, zmax = 50000000,   # Set consistent color scale limits
      
      # Custom hover text
      hovertemplate = paste(
        "<b>Country:</b> %{x}<br>",
        "<b>Citizenship:</b> %{y}<br>",
        "<b>Total Employment:</b> %{z}<br>",
        "<extra></extra>"  # Removes default trace info
      )
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
  interactive_heatmap_2010 <- create_plotly_heatmap("2010-01-01", showlegend = TRUE)
  interactive_heatmap_2023 <- create_plotly_heatmap("2023-01-01", showlegend = FALSE)
  
  # Combine the interactive heatmaps vertically
  combined_interactive_heatmaps <- subplot(
    interactive_heatmap_2010,
    interactive_heatmap_2023,
    nrows = 2,
    shareX = TRUE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    layout(
      annotations = list(
        list(
          text = "<b>Year: 2010 (after 2008 Financial Crisis)</b>",
          x = 0.5,
          y = 1.06,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, family = "Arial")
        ),
        list(
          text = "<b>Year: 2023 (after COVID-19 Pandemic)</b>",
          x = 0.5,
          y = 0.50,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, family = "Arial")
        )
      ),
      title = "<b>Employment Trends by Citizenship Category and Country (2010 vs 2023)</b>",
      margin = list(l = 100, r = 50, t = 80, b = 100),
      titlefont = list(size = 20, family = "Arial", color = "black")
    )
  
  # Display the combined interactive heatmaps
  combined_interactive_heatmaps
}


