# load packages

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(janitor)
library(dplyr)
library(stringr)


# load cvs files

drugs_df <- read_csv("data/raw/Substance_Use_20250725.csv", show_col_types = FALSE)
naloxone_df <- read_csv("data/raw/Naloxone_Administrations_20250725.csv", show_col_types = FALSE)


#Fix column "Naxolone Administrations" name

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

naloxone_df <- naloxone_df %>%
  mutate(
    dispatch_day = as.Date(dispatch_date) #create new day variable to ignore time
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
cols_to_concat2 <- c("age", "gender", "ward",  "neighbourhood")

#creates summary df -> groups by incident for naloxone_df
naloxone_summary <- naloxone_df %>%
  group_by(incident_number, neighbourhood_id, dispatch_day) %>%
  summarise(
    across(all_of(cols_to_concat2), ~ 
             if (n_distinct(.) == 1) as.character(first(.)) 
           else str_c(unique(.), collapse = ";")),
    num_patients = n(),
    naloxone_given = sum(naloxone_adm, na.rm = TRUE),
    num_patients = n_distinct(patient_number),
    avg_doses_per_patient = sum(naloxone_adm, na.rm = TRUE) / n_distinct(patient_number),
    .groups = "drop"
  )

# naloxone_summary2 %>%
#   count(incident_number, neighbourhood_id, dispatch_day) %>%
#   filter(n > 1)


#show lines with repeated composed key -> multiple-patient incident
drugs_df %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)#

#variables list to summary df for drugs
cols_to_concat <- c("age", "gender", "substance", "ward",  "neighbourhood")


#creates summary df -> groups by incident for drugs_df
drugs_summary <- drugs_df %>%
  group_by(incident_number, neighbourhood_id, dispatch_day) %>%
  summarise(
    across(all_of(cols_to_concat), ~ 
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



# ───────────────────────────────────────────────────────────────
# STEP 4: Summarize naloxone administrations
# - Group by incident keys
# - Calculate total naloxone given per incident
# ───────────────────────────────────────────────────────────────

# drugs_df %>%
#   filter(incident_number == "2011008470")  # exemplo com 3 linhas
# 
# 
# incident_data_dedup <- drugs_df %>%
#   distinct(incident_number, neighbourhood_id, dispatch_date, .keep_all = TRUE)
# 
# incident_data_dedup %>%
#   filter(incident_number == "2011008470")  # exemplo com 3 linhas
# ───────────────────────────────────────────────────────────────
# STEP 5: Merge both datasets using a left join
# - Keep all incidents from drugs_df
# - Attach naloxone summary where matched
# - Create logical column: naloxone_administered = TRUE/FALSE
# ───────────────────────────────────────────────────────────────

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
      num_patients.x != num_patients.y
  ) %>%
  select(
    incident_number,  # ou outra ID se tiver
    age.x, age.y,
    gender.x, gender.y,
    neighbourhood.x, neighbourhood.y,
    num_patients.x, num_patients.y
  )

df_diferencas <- merged_df %>%
  filter(
    str_trim(age.x)            != str_trim(age.y) |
      str_trim(gender.x)         != str_trim(gender.y) |
      str_trim(neighbourhood.x)  != str_trim(neighbourhood.y) |
      num_patients.x             != num_patients.y
  )

merged_df <- merged_df %>%
  mutate(
    age = coalesce(age.x, age.y),
    gender = coalesce(gender.x, gender.y),
    neighbourhood = coalesce(neighbourhood.x, neighbourhood.y),
    num_patients = coalesce(num_patients.x, num_patients.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

# ───────────────────────────────────────────────────────────────
# STEP 6: Save final processed data for static use
# - Can be loaded by Shiny or served via API later
# ───────────────────────────────────────────────────────────────

write_csv(merged_df, "data/processed/merged_incident_data.csv")


# Quantas linhas tinha antes e depois?
nrow(naloxone_df)  # número de pacientes
nrow(merged_df)  # esperado: igual ou maior (se join 1:m)

# Verifique se vieram colunas NA do dataset da direita
summary(merged_df)


nrow(naloxone_df)
nrow(merged_df)


naloxone_summary %>%
  group_by(incident_number) %>%
  summarise(
    total_doses = sum(naloxone_given, na.rm = FALSE)
  )

drugs_df %>%
  count(incident_number) %>%
  filter(n > 1)

merged_df %>%
  group_by(incident_number) %>%
  summarise(
    total_doses = sum(naloxone_given, na.rm = TRUE),
    num_patients = n(),
    avg_doses_per_patient = total_doses / num_patients
  ) %>%
  filter(total_doses > 0 & num_patients >1)

naloxone_df %>%
  filter(incident_number == 2012009713) %>%
  select("incident_number", "neighbourhood_id", "dispatch_date", patient_number, naloxone_adm)

merged_df %>%
  filter(incident_number == 2012009713) %>%
  select("incident_number", "neighbourhood_id", "dispatch_day",  naloxone_adm)

merged_df <- merged_df %>%
  mutate(
    x$incident_number = as.character(x$incident_number),
    y$incident_number = as.character(y$incident_number),

  )

comparison <- merged_df %>%
  filter(incident_number == 2012009713) %>%
  select(incident_number, neighbourhood_id, dispatch_day) %>%
  mutate(dispatch_day = as.Date(dispatch_day)) %>%
  left_join(
    naloxone_df %>%
      filter(incident_number == 2012009713) %>%
      mutate(dispatch_day = as.Date(dispatch_date)) %>%
      mutate(incident_number = as.character(incident_number)) %>%
      select(incident_number, neighbourhood_id, dispatch_day) %>%
      distinct() %>%
      mutate(in_naloxone = TRUE),
    by = c("incident_number", "neighbourhood_id", "dispatch_day")
  )

merged_df %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)
summary(merged_df)
