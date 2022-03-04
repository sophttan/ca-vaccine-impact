# COVID-19 vaccination and public health impact in California

This repository contains analytic code for the COVID-19 vaccination and public health impact in California analysis. Using case data from the California Department of Public Health (CDPH) and public vaccine administraton data (available [here](https://data.chhs.ca.gov/dataset/vaccine-progress-dashboard/resource/faee36da-bd8c-40f7-96d4-d8f283a12b0a)), we estimated the total number of COVID-19 cases, hospitalizations, and deaths averted due to COVID-19 vaccination in California. The results and analytic approaches are generalizable and applicable across the United States.

We used two independent approaches to predict cases that would have occurred in the absence of vaccination in the vaccine era of the COVID-19 pandemic in California. We compared our predictions to the observed number of confirmed cases over time to predict the number of averted cases. The primary model used weekly cases in the unvaccinated population (<12 years) as a proxy for cases in the vaccine-eligible populations (12-17 years, 18-49 years, 50-64 years, and 65+ years) in the absence of vaccination. The alternative approach directly incorporated vaccination data and the estimated risk of infection in each vaccine-eligible age group to predict cases in the absence of vaccination.

We estimated vaccine-averted hospitalizations and deaths by applying monthly hospitalization and death risks to estimates of averted COVID-19 cases.

## Structure
- `src`: contains all code needed for initial cleaning of data and all analyses
  - `1-data cleaning`: contains code for initial cleaning of COVID-19 case data and COVID-19 vaccination data
  - `2-cases`: contains code for the primary and alternative models developed to estimated vaccine-averted COVID-19 cases and accompanying sensitivity analyses
  - `3-hospitalizations and deaths`: contains code for estimating vaccine-averted COVID-19 hospitalizations and deaths
- `data`: contains intermediate datasets 
- `results`: contains all results that can be used to reproduce figures and tables
- `figure scripts`: contains code for reproducing figures
- `figures`: contains figure outputs
- `table scripts`: contains code for reproducing tables
- `tables`: contains table outputs (csv) 
