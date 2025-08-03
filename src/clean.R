# ------------------------------------------------------------------------------
# Author: Giulia Sepeda
# GitHub: https://github.com/giuusepeda
# Project: shiny_substance_use_winnipeg
# File: clean.R
# Created: 26/07/2025
# Description: ETL from https://data.winnipeg.ca/Fire-and-Paramedic-Service/Naloxone-Administrations/qd6b-q49i/about_data
# and https://data.winnipeg.ca/Fire-and-Paramedic-Service/Substance-Use/6x82-bz5y/data_preview
#
# 🚫 This code is part of a personal portfolio project.
# Unauthorized use, copying, or distribution is prohibited.
# For inquiries: giuliasepeda@gmail.com
# ------------------------------------------------------------------------------


# load packages

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(janitor)
library(dplyr)
library(stringr)


# load csv files

drugs_df <- read_csv("data/raw/Substance_Use_20250725.csv", show_col_types = FALSE)
naloxone_df <- read_csv("data/raw/Naloxone_Administrations_20250725.csv", show_col_types = FALSE)


#Fix column 'Naxolone Administrations' name

naloxone_df <- naloxone_df %>%
  rename(naloxone_adm = "Naxolone Administrations")



# standardize columns in drugs_df


naloxone_df <- naloxone_df %>% clean_names()
drugs_df <- drugs_df %>% clean_names()


# check dispatch_date format

head(drugs_df$dispatch_date, 5)
str(drugs_df$dispatch_date)

# drogs chr [1:102650] wrong 06/08/2022 06:57:13 AM

head(naloxone_df$dispatch_date, 5)
str(naloxone_df$dispatch_date)

# naloxone POSIXct[1:24243] right 


# convert keys to character for safe joining

#force conversion and tag fail

drugs_df <- drugs_df %>%
  mutate(
     # 1. cleans extra spaces
    dispatch_date_clean = str_trim(dispatch_date),
     
     # 2. tries to convert to POSIXct
    dispatch_date_parsed = suppressWarnings(mdy_hms(dispatch_date_clean)),
     
     # 3. tags failed ones as true
    parse_failed = is.na(dispatch_date_parsed) & !is.na(dispatch_date_clean)
    )

#check new format
head(drugs_df$dispatch_date_parsed, 5)

#check how many failed (false)
summary(drugs_df$parse_failed) 

#create new day variable to ignore time
naloxone_df <- naloxone_df %>%
  mutate(
    dispatch_day = as.Date(dispatch_date) 
  )

drugs_df <- drugs_df %>%
  mutate(
    dispatch_day = as.Date(dispatch_date_parsed) #create new day variable to ignore time
  )

summary(naloxone_df$dispatch_day)
summary(drugs_df$dispatch_day)


#show lines with repeated composed key -> multiple-patient incident
naloxone_df %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)

#variables list to summary df for naloxone_df
cols_to_concat <- c("age", "gender", "ward", "neighbourhood")

#creates summary df -> groups by incident for naloxone_df
naloxone_summary <- naloxone_df %>%
  group_by(incident_number, neighbourhood_id, dispatch_day) %>%
  summarise(
    across(all_of(cols_to_concat), ~ 
             if (n_distinct(.) == 1) as.character(first(.)) 
           else str_c(unique(.), collapse = ";")),
    num_patients = n(),
    naloxone_given = sum(naloxone_adm, na.rm = TRUE),
    num_patients = n_distinct(patient_number),
    avg_doses_per_patient = sum(naloxone_adm, na.rm = TRUE) / n_distinct(patient_number),
    .groups = "drop"
  )

# naloxone_summary2 %>%
#   count("composite_key") %>%
#   filter(n > 1)


#show lines with repeated composed key -> multiple-patient incident
drugs_df %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)

#variables list to summary df for drugs
cols_to_concat2 <- c("age", "gender", "substance", "ward",  "neighbourhood")


#creates summary df -> groups by incident for drugs_df
drugs_summary <- drugs_df %>%
  group_by(incident_number, neighbourhood_id, dispatch_day) %>%
  summarise(
    across(all_of(cols_to_concat2), ~ 
             if (n_distinct(.) == 1) as.character(first(.)) 
           else str_c(unique(.), collapse = ";")),
    num_patients = n(),
    .groups = "drop"
  )


# Standardize keys in naloxone_summary and drugs_summary

naloxone_summary <- naloxone_summary %>%
  mutate(
    incident_number = as.character(incident_number),
    neighbourhood_id = as.character(neighbourhood_id),
    dispatch_day = as.Date(dispatch_day)
  )

drugs_summary <- drugs_summary %>%
  mutate(
    incident_number = as.character(incident_number),
    neighbourhood_id = as.character(neighbourhood_id),
    dispatch_day = as.Date(dispatch_day)
  )

# Merge both datasets using a left join

merged_df <- left_join(
  drugs_summary,
  naloxone_summary,
  by = c("incident_number", "neighbourhood_id", "dispatch_day")
) %>%
  mutate(
    naloxone_given = replace_na(naloxone_given, 0),
    naloxone_adm = naloxone_given > 0
  )
summary(merged_df)

# Casos com correspondência (naloxone_adm == TRUE)
sum(merged_df$naloxone_adm)  # deve dar 7416

# Casos sem correspondência (linha original, mas sem naloxone info)
sum(!merged_df$naloxone_adm)  # deve dar 88784


# unify differences between what was on drugs X and nalaxone y
merged_df %>%
  mutate(
    age_match = age.x == age.y,
    gender_match = gender.x == gender.y,
    ward_match = ward.x == ward.y,
    neighbourhood_match = neighbourhood.x == neighbourhood.y,
    num_patients_match = num_patients.x == num_patients.y
  ) %>%
  summarise(
    age_all_match = all(age_match, na.rm = TRUE),
    gender_all_match = all(gender_match, na.rm = TRUE),
    ward_all_match = all(ward_match, na.rm = TRUE),
    neighbourhood_all_match = all(neighbourhood_match, na.rm = TRUE),
    num_patients_all_match = all(num_patients_match, na.rm = TRUE)
  )


merged_df %>%
  filter(
    age.x != age.y |
      gender.x != gender.y |
      neighbourhood.x != neighbourhood.y |
      num_patients.x != num_patients.y |
      ward.x != ward.y
  ) %>%
  select(
    incident_number, neighbourhood_id, dispatch_day,  # chave composta
    age.x, age.y,
    gender.x, gender.y,
    neighbourhood.x, neighbourhood.y,
    num_patients.x, num_patients.y,
    ward.x, ward.y
  )

merged_df %>%
  filter(
    age.x != age.y |
      gender.x != gender.y |
      neighbourhood.x != neighbourhood.y |
      num_patients.x != num_patients.y |
      ward.x != ward.y
  ) %>%
  nrow()

anyDuplicated(merged_df$incident_number)


merged_df <- merged_df %>%
  mutate(
    age = coalesce(age.x, age.y),
    gender = coalesce(gender.x, gender.y),
    neighbourhood = coalesce(neighbourhood.x, neighbourhood.y),
    ward = coalesce(ward.x, ward.y),
    num_patients = coalesce(num_patients.x, num_patients.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))


# fill na with "Unknown" for age, gender, substance, neighbourhood

merged_df <- merged_df %>%
  mutate(
    age = if_else(is.na(age), "Unknown", age),
    gender = if_else(is.na(gender), "Unknown", gender),
    substance = if_else(is.na(substance), "Unknown", substance),
    neighbourhood = if_else(is.na(neighbourhood), "Unknown", neighbourhood),
    ward = if_else(is.na(ward), "Unknown", ward)
  )

# change class for factor 
merged_df <- merged_df %>%
  mutate(
    age = factor(age),
    gender = factor(gender),
    substance = factor(substance),
    neighbourhood = factor(neighbourhood),
    ward = factor(ward)
  )


# Verifique se vieram colunas NA do dataset da direita
summary(merged_df)

colSums(is.na(merged_df))

#save as csv

write_csv(merged_df, "data/processed/merged_incident_data.csv")


summary(drugs_df)
summary(merged_df)


