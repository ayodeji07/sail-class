## Set working directory
setwd("C:/Users/Open user/Desktop/SAIL-DS")


## Load Library
#library(readr)
library(tidyverse)
install.packages("readxl")

### Read Metadata
Metadata_Country <- read_excel("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls", sheet = 2)
Metadata_Indicator <- read_excel("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls", sheet = 3)

View(Metadata_Country)
#View(Metadata_Indicator)


## Import data
# Install and load readxl if not already installed

library(readxl)
my_data <- read_excel("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls", sheet = 1, skip = 3)

# Select columns that are not "Indicator Name" and "Indicator Code"
my_data <- my_data %>%
  select(-c(`Indicator Name`, `Indicator Code`, `Country Code`))

View(my_data)

### Create a vector for african countries

# Create a vector with the names of African countries
african_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
  "Congo, Dem. Rep.", "Congo, Rep.", "Djibouti", 
  "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", 
  "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", 
  "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
  "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
  "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", 
  "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
  "Uganda", "Zambia", "Zimbabwe"
)

### Filtered african countries
filtered_data <- my_data %>% filter(`Country Name` %in% african_countries)
filtered_data <- filtered_data %>% arrange(`Country Name`)

view(filtered_data)


