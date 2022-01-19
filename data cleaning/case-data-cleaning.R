###################################################################################################
#Title: CDPH COVID Data Cleaning
#Authors: Sophia Tan and Hailey Park
###################################################################################################

rm(list=ls())
setwd(here::here())

#Loading in libraries
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(tibble)
library(lubridate)
library(scales)
library(stringr)

#Loading in datasets
#Data was obtained from CDPH-not publicly available-available upon request to CDPH
covid_data <- read_csv("Data/covid_registry_2021-10-17.csv")

#Downsizing COVID dataset
covid_selected_data <- covid_data %>%
  select(age, dob, cntyofresid, dtdeath, dtepisode, incidentid, cste_def, county, died, gc_zip, gc_tract)


####################################################################################################
#COVID Dataset Observations

#1. Number of Rows
print(paste0("Total Number of Observations: ", nrow(covid_selected_data)))
nrow(covid_selected_data %>% filter(cste_def == "Confirmed"))
nrow(covid_selected_data %>% filter(!is.na(cste_def)))

#2. Number of Rows omitting NAs
print(paste0("Total Number of Observations (omit NAs): ", nrow(na.omit(covid_selected_data))))


####################################################################################################
#COVID Dataset Cleaning—NEW (10/19/21)

#De-selecting 'cntyofresid' and 'county' and 'gc_zip' because 'gc_tract' has geocodes for tract
covid_data_clean <- covid_selected_data %>%
  dplyr::select(!c(cntyofresid, gc_zip))

#Converting 'dtepisode' variable to Date objects
covid_data_clean$dtepisode <- as.Date(covid_data_clean$dtepisode)
covid_data_clean$dtdeath <- as.Date(covid_data_clean$dtdeath)


#Filtering only 'Confirmed' cases for 'cste_def' variable
covid_data_clean <- covid_data_clean %>%
  filter(cste_def == 'Confirmed')

###CHECK ROWS###
print(nrow(covid_data_clean))


jan_1 <- as.Date("2020-01-01", format = "%Y-%m-%d")
covid_data_clean <- covid_data_clean %>%
  mutate(weeks_since_Jan2020 = round(as.vector(difftime(as.Date(dtepisode, format = "%Y-%m-%d"), jan_1, units = "weeks"))),
         months_since_Jan2020 = lubridate::interval(jan_1, as.Date(dtepisode, format = "%Y-%m-%d")) %/% months(1))

covid_data_clean %>% head()

# manually calculate age based on DOB for consistency
with_age <- covid_data_clean %>%
  mutate(age_hand = as.numeric(floor(lubridate::time_length(difftime(dtepisode, dob), "years"))))
with_age %>% filter(abs(age_hand - age) != 0) %>% nrow() # 125k (<3% of data don't match based on reported and calculated age)

# cases by age group over time
breaks <- c(0, 12, 18, 50, 65, Inf)
breaks_inf <- c(0, 12, 18, 19, 50, 60, 65, Inf)
cases <- with_age %>% mutate(age_hand_cut = cut(age_hand, breaks=breaks, right = FALSE),
                             age_hand_cut_inf = cut(age_hand, breaks=breaks_inf, right = FALSE)) %>%
  filter(!is.na(age_hand_cut))
ca_cases <- cases %>% group_by(weeks_since_Jan2020, age_hand_cut) %>%
  summarise(cases=n()) %>% ungroup() %>% spread(age_hand_cut, cases, fill=0)
ca_cases <- ca_cases %>% mutate(cases=rowSums(.[,2:ncol(.)]))
ca_cases

write.csv(ca_cases, "data/ca_case_data.csv", row.names = F)

# create dataset using breaks_inf to calculate total infections in alternative analysis
ca_cases_inf <- cases %>% group_by(weeks_since_Jan2020, age_hand_cut_inf) %>%
  summarise(cases=n()) %>% ungroup()
ca_cases_inf

write.csv(ca_cases_inf, "data/ca_case_data_infection_cutoffs.csv", row.names = F)


####################################################################################################
# CLEAN DATES FOR TIME LABELS
# map weeks to months for cleaner labels of time
dates <- covid_data_clean %>% group_by(weeks_since_Jan2020) %>%
  summarise(months = round(mean(months_since_Jan2020)))
dates

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
months <- data.frame(month_num=0:23,
                     month = c(paste(month_names, 20),
                               paste(month_names, 21)))

dates <- dates %>% merge(months, by.x="months", by.y="month_num")
dates

dates2 <- dates %>% group_by(month) %>% summarise(weeks = first(weeks_since_Jan2020)) %>% arrange(weeks)

write.csv(dates2, file="data/weeks_months_data.csv")

