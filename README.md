# 📊 Substance Use & Naloxone Incidents — Data Preparation (Winnipeg)

This project focuses on building the **data preparation and integration layer** for analyzing emergency incidents involving substance use and naloxone administration from the Winnipeg Fire & Paramedic Services.

The current stage of the project is centered on **ETL and data quality processing**, forming the foundation for future analytical and dashboard development.

---

## 🔗 Data Sources

- [Naloxone Administrations Dataset](https://data.winnipeg.ca/Fire-and-Paramedic-Service/Naloxone-Administrations/qd6b-q49i)
- [Substance Use Dataset](https://data.winnipeg.ca/Fire-and-Paramedic-Service/Substance-Use/6x82-bz5y)

---

## ⚙️ Current Scope

This repository currently focuses on:

- Data extraction from raw CSV sources
- Cleaning and standardizing column names and text fields
- Datetime parsing and validation (handling inconsistent formats)
- Creation of a **composite key** (`incident_number`, `neighbourhood_id`, `dispatch_day`)
- Incident-level aggregation (handling multi-patient records)
- Dataset merging using left join
- Missing value handling and standardization ("Unknown")
- Cross-dataset validation and consistency checks
- Reconciliation of duplicated fields after merging
- Export of a clean, analysis-ready dataset

---

## 🧪 Data Processing Highlights

- Identified and handled datetime parsing failures
- Aggregated multiple patient records into single incident-level entries
- Validated consistency across datasets (age, gender, ward, neighbourhood, patient count)
- Resolved duplicated columns (`.x` / `.y`) using `coalesce()`
- Ensured structural integrity of the merged dataset

---

## 📦 Output

Processed dataset:
'data/processed/merged_incident_data.csv'

This dataset is:
- Cleaned
- Standardized
- Aggregated at the incident level
- Ready for downstream analysis and visualization

---

## 🛠️ Tech Stack

- **R**
- `tidyverse` (dplyr, readr, stringr)
- `lubridate`
- `janitor`

---

## 🚧 Future Work

- Exploratory data analysis (EDA)
- Trend analysis over time
- Demographic breakdowns
- Geospatial visualization
- Interactive dashboard using Shiny
- Automated data pipeline (GitHub Actions)
- Containerization (Docker)

---

## 📬 Contact

Giulia Sepeda  
[GitHub](https://github.com/giuuusepeda) | [Portfolio](https://giuliasepeda.carrd.co)

---

## ⚠️ Usage & License

This repository is intended for **personal portfolio purposes only**.

The content is licensed under the  
**Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0)**.

You are allowed to:
- View and share the project as-is
- Credit the original author

You are **not allowed to**:
- Use this work for commercial purposes
- Copy or modify the source code
- Distribute derivative works

🔗 Full license: https://creativecommons.org/licenses/by-nc-nd/4.0/
