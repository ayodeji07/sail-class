## Set working directory
setwd("C:/Users/Open User/Desktop/Nurudeen-DS/SAIL-Project")


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
  select(-c(`Indicator Name`, `Indicator Code`))

View(my_data)
