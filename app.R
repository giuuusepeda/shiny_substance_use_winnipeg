# Load required libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(janitor)


# ───────────────────────────────────────────────────────────────
# STEP 1: Load raw CSVs
# ───────────────────────────────────────────────────────────────

drugs_df <- read_csv("data/raw/Substance_Use_20250725.csv", show_col_types = FALSE)
naloxone_df <- read_csv("data/raw/Naloxone_Administrations_20250725.csv", show_col_types = FALSE)

# ───────────────────────────────────────────────────────────────
# STEP 2: Standardize columns in drugs_df
# - Convert keys to character for safe joining
# - Parse Dispatch Date from text to datetime
# ───────────────────────────────────────────────────────────────



naloxone_df <- naloxone_df %>% clean_names()
drugs_df <- drugs_df %>% clean_names()


drugs_df <- drugs_df %>%
  mutate(
    `incident_number` = as.character(`incident_number`),
    `neighbourhood_id` = as.character(`neighbourhood_id`),
    `dispatch_date` = parse_date_time(`dispatch_date`, orders = "mdy IMS p")
  )


# ───────────────────────────────────────────────────────────────
# STEP 3: Standardize keys in naloxone_df
# - Keep Dispatch Date as-is (already parsed)
# ───────────────────────────────────────────────────────────────

naloxone_df <- naloxone_df %>%
  mutate(
    `incident_number` = as.character(`incident_number`),
    `neighbourhood_id` = as.character(`neighbourhood_id`)
  )

# ───────────────────────────────────────────────────────────────
# STEP 4: Summarize naloxone administrations
# - Group by incident keys
# - Calculate total naloxone given per incident
# ───────────────────────────────────────────────────────────────

naloxone_summary <- naloxone_df %>%
  group_by(`incident_number`, `neighbourhood_id`, `dispatch_date`) %>%
  summarise(
    naxolone_given = sum(`naxolone_administrations`, na.rm = TRUE),
    .groups = "drop"
  )

# ───────────────────────────────────────────────────────────────
# STEP 5: Merge both datasets using a left join
# - Keep all incidents from drugs_df
# - Attach naloxone summary where matched
# - Create logical column: naloxone_administered = TRUE/FALSE
# ───────────────────────────────────────────────────────────────

merged_df <- left_join(
  drugs_df,
  naloxone_summary,
  by = c("incident_number", "neighbourhood_id", "dispatch_date")
) %>%
  mutate(
    naxolone_given = replace_na(naxolone_given, 0),
    naloxone_administered = naxolone_given > 0
  )

# ───────────────────────────────────────────────────────────────
# STEP 6: Save final processed data for static use
# - Can be loaded by Shiny or served via API later
# ───────────────────────────────────────────────────────────────

write_csv(merged_df, "data/processed/merged_incident_data.csv")



# Run the application 
#shinyApp(ui = ui, server = server)

