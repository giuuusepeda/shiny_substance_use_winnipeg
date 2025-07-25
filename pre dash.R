total_incidents <- nrow(merged_df)
total_with_naloxone <- sum(merged_df$naloxone_adm, na.rm = TRUE)
total_naloxone_doses <- sum(merged_df$naloxone_given, na.rm = TRUE)
total_patients <- sum(merged_df$num_patients.y, na.rm = TRUE)
avg_doses <- mean(merged_df$avg_doses_per_patient, na.rm = TRUE)



drugs_df %>%
  filter(incident_number == "2021093714") %>%
  select(incident_number, neighbourhood_id, dispatch_date, age, gender, substance)

naloxone_df %>%
  filter(incident_number == "2021093714") %>%
  select(incident_number, neighbourhood_id, dispatch_date, age, gender, naloxone_adm)

merged_df %>%
  filter(incident_number == "2021093714") %>%
  select(incident_number, neighbourhood_id, dispatch_day, dispatch_day, num_patients.y, num_patients.x, age, gender, substance, naloxone_given, num_patients.y, avg_doses_per_patient)
