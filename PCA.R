# Load the necessary libraries
library(tidyverse)     # For data manipulation and visualization
library(eurostat)      # For downloading Eurostat data
library(FactoMineR)    # For performing PCA
library(factoextra)    # For visualizing PCA results

# Download the 'lfsa_egan' dataset from Eurostat
data <- get_eurostat("lfsa_egan")

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

unique(data$citizen)
# [1] "EU27_2020_FOR"  "FOR"            "NAT"            "NEU27_2020_FOR" "NRP"            "STLS"           "TOTAL"         

unique(data$geo)
# [1] "AT"        "BE"        "CH"        "CY"        "CZ"        "DE"        "DK"        "EA20"      "EE"        "EL"        "ES"       
# [12] "EU27_2020" "FI"        "FR"        "HU"        "IE"        "IS"        "IT"        "LU"        "ME"        "MT"        "NL"       
# [23] "NO"        "PT"        "RS"        "SE"        "SI"        "SK"        "UK"        "BG"        "HR"        "LT"        "LV"       
# [34] "MK"        "PL"        "RO"        "BA"        "TR"       

# Transform the data:
# - Create a wide-format dataset with countries as rows and citizen categories as columns.
# - Fill missing values with 0.
pca_data <- data_filtered %>%
  pivot_wider(names_from = citizen, values_from = values, values_fill = 0)

# Prepare the data for PCA:
# - Remove non-numeric columns ('geo', 'age', 'sex', 'unit', 'freq', 'TIME_PERIOD')
pca_input <- pca_data %>% select(-geo, -age, -sex, -unit, -freq, -TIME_PERIOD)

# Standardize the data:
# - PCA requires data to be scaled (mean = 0, standard deviation = 1).
pca_data_scaled <- scale(pca_input)

# Perform PCA on the standardized data
pca_result <- PCA(pca_data_scaled, graph = FALSE)

# Check the dimensions of the input data
print(dim(pca_input))  # Should display the number of rows (countries) and columns (features)
print(length(pca_data$geo))  # Ensure it matches the number of rows in pca_input

# Visualize the explained variance:
# - Scree plot to determine how many principal components to retain
fviz_eig(pca_result)

# > pca_result$eig
# eigenvalue percentage of variance cumulative percentage of variance
# comp 1 5.843290e+00           8.347556e+01                          83.47556
# comp 2 1.008833e+00           1.441191e+01                          97.88747
# comp 3 1.097596e-01           1.567994e+00                          99.45547
# comp 4 3.049431e-02           4.356330e-01                          99.89110
# comp 5 7.622619e-03           1.088946e-01                          99.99999
# comp 6 5.037309e-07           7.196156e-06                         100.00000
# comp 7 6.907112e-11           9.867303e-10                         100.00000
# The first principal component (Dim.1) explains 83.48% of the variance.
# The second principal component (Dim.2) explains an additional 14.41%.
# Together, Dim.1 and Dim.2 account for 97.89% of the variance, meaning most of the data's structure can be captured by these two components.

# > pca_result$var$contrib
# Dim.1        Dim.2      Dim.3      Dim.4        Dim.5
# EU27_2020_FOR  16.9449042  0.016856241  0.7600122 14.9053527 5.655026e+01
# FOR            17.0227057  0.012518695  0.1350362 16.3810041 5.760064e-01
# NAT            16.7043133  0.012792702 15.2136644 23.1883796 2.619386e-01
# NEU27_2020_FOR 16.9414205  0.009805109  1.4047309 17.4420376 4.072171e+01
# NRP             0.0493857 98.815650523  0.1450972  0.2282598 3.210149e-04
# STLS           15.7310356  0.004087743 69.9559694 12.5689501 1.739918e+00
# TOTAL          16.6062351  1.128288986 12.3854897 15.2860161 1.498498e-01
# Dim.1 captures general trends across multiple citizen categories, each contributing around 17%.
# Dim.2 is dominated by the NRP category, which contributes 98.82%. 
# This suggests that variations in the Non-Resident Population (NRP) are distinct and captured almost entirely by the second component.

# Convert 'geo' (country) to a factor for coloring in plots
pca_data$geo <- as.factor(pca_data$geo)

# Visualize the PCA results:
# - Plot individuals (countries) colored by 'geo'
# - Add ellipses to group countries by their categories
fviz_pca_ind(pca_result, label = "none", habillage = pca_data$geo, addEllipses = TRUE)

# Contributions to the first component
fviz_contrib(pca_result, choice = "var", axes = 1)
# Contributions to the second component
fviz_contrib(pca_result, choice = "var", axes = 2)
# Contribution of each variable to the principal components
fviz_contrib(pca_result, choice = "var", axes = 1:2)
