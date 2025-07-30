# 📊 Substance Use Dashboard — MVP

This is a work-in-progress Shiny dashboard exploring substance use and naloxone administration data from the Winnipeg Fire & Paramedic Services.

## 🔗 Data Sources

- [Naloxone Administrations Dataset](https://data.winnipeg.ca/Fire-and-Paramedic-Service/Naloxone-Administrations/qd6b-q49i)
- [Substance Use Dataset](https://data.winnipeg.ca/Fire-and-Paramedic-Service/Substance-Use/6x82-bz5y)

## 🛠 Features (in development)

- Merges datasets by `incident_number`, `dispatch_date`, and `neighbourhood_id`
- Interactive filters for:
  - Time (month, weekday, hour of day)
  - Demographics (age, gender)
  - Substance type
- Incident trends and population pyramid
- Map of substance use services (local and virtual) — _planned_

## ⚙️ Tech Stack

- `R`, `Shiny`, `tidyverse`
- `plotly` (optional interactive charts)
- GitHub Actions (for automatic updates) — _in progress_
- Docker (for reproducibility) — _planned_

## 🚧 Status

This dashboard is under development as part of my personal portfolio and freelance application.  
Some visual and content issues are expected and will be addressed in the next iterations.

## 📬 Contact

Giulia Sepeda  
[GitHub](https://github.com/giuuusepeda) | [Portfolio](https://giuliasepeda.carrd.co)

## ⚠️ Usage & License

This repository is intended for **personal portfolio purposes only**.  
**Unauthorized use, redistribution, or copying of any part of this content is strictly prohibited.**

The content is licensed under the **Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0)**.

You are allowed to:
- View and share the project as-is
- Credit the original author (Giulia Sepeda)

You are **not allowed to**:
- Use this work for commercial purposes
- Copy or modify the source code
- Distribute any derivative works

🔗 Full license text: [https://creativecommons.org/licenses/by-nc-nd/4.0/](https://creativecommons.org/licenses/by-nc-nd/4.0/)
