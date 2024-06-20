## Set working directory
setwd("C:/Users/Open User/Desktop/Nurudeen-DS/SAIL-Project")


## Load Library
library(readr)

### Read Metadata

Metadata_Country <- read_csv("Metadata_Country_API_SM.POP.NETM_DS2_en_csv_v2_434367.csv")
Metadata_Indicator <- read_csv("Metadata_Indicator_API_SM.POP.NETM_DS2_en_csv_v2_434367.csv")


View(Metadata_Country)
View(Metadata_Indicator)
