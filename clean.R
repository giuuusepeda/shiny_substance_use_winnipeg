library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(janitor)
library(dplyr)
library(stringr)



# ───────────────────────────────────────────────────────────────
# STEP 1: Load raw CSVs
# ───────────────────────────────────────────────────────────────

drugs_df <- read_csv("data/raw/Substance_Use_20250725.csv", show_col_types = FALSE)
naloxone_df <- read_csv("data/raw/Naloxone_Administrations_20250725.csv", show_col_types = FALSE)

naloxone_df <- naloxone_df %>%
  rename(naloxone_adm = "Naxolone Administrations")
# ───────────────────────────────────────────────────────────────
# STEP 2: Standardize columns in drugs_df
# - Convert keys to character for safe joining
# - Parse Dispatch Date from text to datetime
# ───────────────────────────────────────────────────────────────



naloxone_df <- naloxone_df %>% clean_names()
drugs_df <- drugs_df %>% clean_names()

head(drugs_df$dispatch_date, 5)
str(drugs_df$dispatch_date)

head(naloxone_df$dispatch_date, 5)
str(naloxone_df$dispatch_date)


drugs_df <- drugs_df %>%
  mutate(
    dispatch_date = mdy_hms(dispatch_date),
    dispatch_day = as.Date(dispatch_date)
  )
naloxone_df <- naloxone_df %>%
  mutate(dispatch_day = as.Date(dispatch_date))

summary(naloxone_df$dispatch_day)
summary(drugs_df$dispatch_day)

naloxone_df %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)




naloxone_summary2 <- naloxone_df %>%
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

naloxone_summary %>%
  count(incident_number, neighbourhood_id, dispatch_day) %>%
  filter(n > 1)



# Lista das colunas que queremos unificar
cols_to_concat <- c("age", "gender", "substance", "ward",  "neighbourhood")
cols_to_concat2 <- c("age", "gender", "ward",  "neighbourhood")

# Agrupamento e concatenação
drugs_summary <- drugs_df %>%
  group_by(incident_number, neighbourhood_id, dispatch_day) %>%
  summarise(
    across(all_of(cols_to_concat), ~ 
             if (n_distinct(.) == 1) as.character(first(.)) 
           else str_c(unique(.), collapse = ";")),
    num_patients = n(),
    .groups = "drop"
  )

# ───────────────────────────────────────────────────────────────
# STEP 3: Standardize keys in naloxone_df
# - Keep Dispatch Date as-is (already parsed)
# ───────────────────────────────────────────────────────────────

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
