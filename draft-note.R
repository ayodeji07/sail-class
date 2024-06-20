## Set working directory
#Ayodeji
#setwd("C:/Users/Open user/Desktop/SAIL-DS")
#Nurudeen
#setwd("C:/Users/Open User/Desktop/Nurudeen-DS/SAIL-Project")


## Load Library
if(!require(pacman)) install.package('pacman')

pacman::p_load(
  tidyverse,
  esquisse,
  readxl,
  skimr
)

### Read Metadata
Metadata_Country <- read_excel("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls", sheet = 2)
Metadata_Indicator <- read_excel("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls", sheet = 3)


View(Metadata_Country)
View(Metadata_Indicator)

#Create wrangle function
wrangle_data <- function(my_data){
  
  #load data
  my_data <- read_excel(my_data, sheet = 1, skip = 3)
  
  # Select columns that are not "Indicator Name" and "Indicator Code"
  my_data <- my_data %>%
    select(-c(`Indicator Name`, `Indicator Code`, `Country Code`))
  
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
  
  return(filtered_data)
}

transpose_data <- function(filtered_data){
  # Transpose the data frame
  tibbled_data <- tibble(filtered_data)
  transposed_data <- tibbled_data %>%
    pivot_longer(cols = -`Country Name`, names_to = "year", values_to = "Value") %>%
    pivot_wider(names_from = `Country Name`, values_from = Value)
  
  # Append "-12-31" to each year to represent December 31st of that year
  transposed_data$date <- as.Date(paste0(transposed_data$year, "-12-31"))
  
  #Put the date column at the beginning and drop the year column
  trans_data <- transposed_data %>% 
    select(-year) %>% 
    select(date, everything())
  
  return(trans_data)
}

filtered_data$total <- filtered_data %>% 
                          select(-c(`Country Name`)) %>% 
                            rowSums()

view(filtered_data)

filtered_data <- wrangle_data("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls")
view(filtered_data)
trans_data <- transpose_data(filtered_data)
view(trans_data)


glimpse(cleaned_data %>% select(date, everything()))
skim(cleaned_data)
view(cleaned_data)

view(cleaned_data %>% select(date, Nigeria))
sum(is.na(cleaned_data))

esquisse::esquisser(trans_data)







ggplot(transposed_data, aes(x = date, y = Algeria)) +
  geom_line() +
  labs(x = "Year", y = "Data Value", title = "Data Trend for Algeria") +
  theme_minimal()




# Print the resulting Date vector
print(year_date)


