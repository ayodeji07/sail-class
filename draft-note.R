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


filtered_data <- wrangle_data("API_SM.POP.NETM_DS2_en_excel_v2_424013.xls")
filtered_data$total <- filtered_data %>% 
  select(-c(`Country Name`)) %>% 
  rowSums()
view(filtered_data)


trans_data <- transpose_data(filtered_data)
view(trans_data)

esquisse::esquisser(filtered_data)
esquisse::esquisser(trans_data)

#EDA

# Test net migration for all the countries
ggplot(filtered_data) +
  aes(x = `Country Name`, y = `total`) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90L))
# Making Nigeria (~800k net imnigration) as the focus, select countries with more 2M net immigration/Emigration
# Immgiration: Ethopia, South Africa
# Emmigration: Mali, Morocco, Susan, Uganda, Zimbabwe


# Get the trend for the selected countries
ggplot(trans_data) +
  aes(x = date, y = Nigeria) +
  geom_line(colour = "#112446") +
  theme_minimal()

ggplot(filtered_data) +
  aes(x = `Country Name`, y = `1983`) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90L))


