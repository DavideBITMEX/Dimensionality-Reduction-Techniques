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

# Geospatial Analysis
data2 <- get_eurostat("lfsi_emp_a")
countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
  "PL", "PT", "RO", "SK", "SI", "ES", "SE", "CH", "IS", "ME", "NO", "RS", "UK", "MK", "BA", "TR")

df <- data2 %>%
  filter(age == "Y15-64",
         geo %in% countries,
         indic_em == "EMP_LFS",
         sex == "T",
         unit == "PC_POP") %>%
         mutate(year = year(TIME_PERIOD)) %>%
  select(geo, year, values)


# Define color classes (example, adjust ranges accordingly)
dta_clss <- list(
  list(from = 0, to = 55, color = "#EBEBEB", name = "<55"),
  list(from = 55, to = 65, color = "#BFD0E4", name = "55-65"),
  list(from = 65, to = 75, color = "#7FA1C9", name = "65-75"),
  list(from = 75, to = 82, color = "#4073AF", name = "75-82"),
  list(from = 82, to = 100, color = "#003776", name = ">82")
)

# Function to generate the map for a specific year
generate_map <- function(year_selected) {
  
  # Filter the data for the selected year
  df_year <- df %>%
    filter(year == year_selected)
  
  # Generate the map
  hc <- hcmap("custom/europe", 
              data = df_year,
              joinBy = c("iso-a2", "geo"),  
              name = "Employment Rate",  
              value = "values",  
              tooltip = list(pointFormat = "{point.value}%"),  
              dataLabels = list(
                enabled = TRUE, 
                format = "{point.value}%"  # Display name and value with "%"
              )) %>%
    hc_colorAxis(dataClassColor = "category", 
                 dataClasses = dta_clss) %>%
    hc_title(text = paste("Employment Rate, ", year_selected)) %>%  # Dynamically update title
    hc_subtitle(text = "(% of people aged 15-64)")
  
  return(hc)
}

# Generate the initial map for 2020 (or any other starting year)
generate_map(2023)






