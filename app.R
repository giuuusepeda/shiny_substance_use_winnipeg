# Load required libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)


# Load raw datasets
drugs_df <- read_csv("data/raw/Substance_Use_20250725.csv")
naloxone_df <- read_csv("data/raw/Naloxone_Administrations_20250725.csv")


# Standardize column types for merging
drugs_df <- drugs_df %>%
  mutate(
    `Incident Number` = as.character(`Incident Number`),
    `Neighbourhood ID` = as.character(`Neighbourhood ID`),
    `Dispatch Date` = parse_date_time(`Dispatch Date`, orders = "mdy IMS p")
  )

naloxone_df <- naloxone_df %>%
  mutate(
    `Incident Number` = as.character(`Incident Number`),
    `Neighbourhood ID` = as.character(`Neighbourhood ID`),
    `Dispatch Date` = floor_date(`Dispatch Date`, unit = "minute")  # round if needed
  )

# Merge using the three columns
merged_df <- full_join(
  drugs_df,
  naloxone_df %>% select(`Incident Number`, `Neighbourhood ID`, `Dispatch Date`, `Naxolone Administrations`),
  by = c("Incident Number", "Neighbourhood ID", "Dispatch Date")
) %>%
  mutate(
    naloxone_administered = if_else(is.na(`Naxolone Administrations`), FALSE, `Naxolone Administrations` > 0)
  )



# Run the application 
#shinyApp(ui = ui, server = server)

