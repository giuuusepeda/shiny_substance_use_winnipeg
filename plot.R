library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)







age_freq <- merged_df %>%
  separate_rows(age, sep = ";") %>%
  filter(!is.na(age), age != "", age != "Unknown") %>%
  mutate(age = str_trim(age))  # remove espaços extras

age_counts <- age_freq %>%
  count(age, sort = TRUE) %>%
  filter(n > 2)  # mostra só faixas com pelo menos 3 ocorrências
library(ggplot2)

# Define a ordem correta
ordem_idades <- c(
  "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
  "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
  "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99", "Over 100"
)

# Converte a coluna age para fator ordenado
age_counts <- age_counts %>%
  mutate(age = factor(age, levels = ordem_idades))



ggplot(age_counts, aes(x = age, y = n)) +
  geom_col() +
  labs(
    title = "Frequência de faixas etárias em ordem cronológica",
    x = "Faixa etária",
    y = "Número de aparições"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


age_freq <- merged_df %>%
  separate_rows(age, sep = ";") %>%
  filter(!is.na(age), age != "", age != "Unknown") %>%
  mutate(age = str_trim(age))

age_by_naloxone <- age_freq %>%
  group_by(age, naloxone_adm) %>%
  summarise(n = n(), .groups = "drop")

ordem_idades <- c(
  "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
  "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
  "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99", "Over 100"
)

age_by_naloxone <- age_by_naloxone %>%
  filter(age %in% ordem_idades) %>%
  mutate(age = factor(age, levels = ordem_idades))




ggplot(age_by_naloxone, aes(x = age, y = n, fill = naloxone_adm)) +
  geom_col() +
  labs(
    title = "Frequência por faixa etária colorido por uso de naloxona",
    x = "Faixa etária",
    y = "Número de aparições",
    fill = "Naloxona administrada"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


subst_por_idade <- merged_df %>%
  separate_rows(age, sep = ";") %>%
  separate_rows(substance, sep = ";") %>%
  filter(
    !is.na(age), age != "", age != "Unknown",
    !is.na(substance), substance != "", substance != "Unknown"
  ) %>%
  mutate(
    age = str_trim(age),
    substance = str_trim(substance)
  )

subst_counts <- subst_por_idade %>%
  count(age, substance, sort = TRUE)
top_subst_by_age <- subst_counts %>%
  group_by(age) %>%
  slice_max(n, n = 5) %>%
  ungroup()
library(ggplot2)

ggplot(top_subst_by_age, aes(x = reorder_within(substance, n, age), y = n, fill = substance)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ age, scales = "free_y") +
  scale_x_reordered() +
  labs(
    title = "Top 5 substâncias por faixa etária",
    x = "Substância",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




age_subst_df <- merged_df %>%
  separate_rows(age, sep = ";") %>%
  separate_rows(substance, sep = ";") %>%
  filter(!is.na(age), age != "", age != "Unknown",
         !is.na(substance), substance != "", substance != "Unknown") %>%
  mutate(
    age = str_trim(age),
    substance = str_trim(substance)
  )
age_subst_counts <- age_subst_df %>%
  count(age, substance)
ordem_idades <- c(
  "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
  "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
  "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99", "Over 100"
)

age_subst_counts <- age_subst_counts %>%
  filter(age %in% ordem_idades) %>%
  mutate(age = factor(age, levels = ordem_idades))


ggplot(age_subst_counts, aes(x = age, y = n, fill = substance)) +
  geom_col() +
  labs(
    title = "Frequência de substâncias por faixa etária",
    x = "Faixa etária",
    y = "Número de incidentes (múltiplas substâncias por incidente)",
    fill = "Substância"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
