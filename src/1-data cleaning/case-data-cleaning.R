###################################################################################################
#Title: CDPH COVID Data Cleaning
#Authors: Sophia Tan and Hailey Park
###################################################################################################

rm(list=ls())
source("configuration.R")

#Loading in datasets
#not publicly available
covid_data <- read_csv("/mnt/projects/covid_partners/ucsf_lo/Data/covid_registry_2021-10-17.csv")

#Downsizing COVID dataset
covid_selected_data <- covid_data %>%
  select(age, dob, dtdeath, dtepisode, cste_def, hospitalized, died)


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

#Converting 'dtepisode' variable to Date objects
covid_selected_data$dtepisode <- as.Date(covid_selected_data$dtepisode)
covid_selected_data$dtdeath <- as.Date(covid_selected_data$dtdeath)


#Filtering only 'Confirmed' cases for 'cste_def' variable
covid_data_clean <- covid_selected_data %>%
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

write.csv(ca_cases, "/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/Data/ca_case_data.csv", row.names = F)

# create dataset using breaks_inf to calculate total infections in alternative analysis
ca_cases_inf <- cases %>% group_by(weeks_since_Jan2020, age_hand_cut_inf) %>%
  summarise(cases=n()) %>% ungroup()
ca_cases_inf

write.csv(ca_cases_inf, "/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/Data/ca_case_data_infection_cutoffs.csv", row.names = F)


write_csv(cases, "/mnt/projects/covid_partners/ucsf_lo/Direct Effects Analysis/Data/ca_case_data_cleaned_full.csv")


####################################################################################################
# CLEAN HOSPITALIZATION AND DEATH DATA IN CASE REGISTRY
hospitalizations_and_deaths <- cases %>% filter(hospitalized=="Y" | died=="Y")

hospitalizations_and_deaths %>% filter(hospitalized=="Y") %>% nrow()
hospitalizations_and_deaths %>% filter(died=="Y") %>% nrow()

summary_data <- cases %>% group_by(weeks_since_Jan2020) %>%
  summarise(cases=n(),
            num_died=sum(died=="Y", na.rm=T), num_hosp=sum(hospitalized=="Y", na.rm=T))

summary_data %>% head()

summary_data_age <- cases %>% group_by(weeks_since_Jan2020, age_hand_cut) %>%
  summarise(months_since_Jan2020 = round(mean(months_since_Jan2020)),
            cases=n(),
            num_died=sum(died=="Y", na.rm=T), num_hosp=sum(hospitalized=="Y", na.rm=T))

summary_data_age_month <- summary_data_age %>% group_by(months_since_Jan2020, age_hand_cut) %>%
  summarise_all(sum) %>% select(!weeks_since_Jan2020)
summary_data_age_month <- summary_data_age_month %>% mutate(hosp_rate = num_hosp/cases*100,
                                                            death_rate = num_died/cases*100)


# overall monthly rates over time
summary_data_age_month %>%
  ggplot(aes(months_since_Jan2020, hosp_rate, group=age_hand_cut, color=age_hand_cut)) +
  geom_line() +
  scale_x_continuous(name = "Month", breaks=0:21) +
  scale_color_discrete(labels=c("<12", "12-17", "18-49", "50-64", "65+")) +
  ylab("Monthly hospitalization risk (%)") + labs(color="Age group (years)") +
  theme(legend.title = element_text("Age group (years)"))

summary_data_age_month %>%
  ggplot(aes(months_since_Jan2020, death_rate, group=age_hand_cut, color=age_hand_cut)) +
  geom_line() +
  scale_x_continuous(name = "Month", breaks=0:21) +
  scale_color_discrete(labels=c("<12", "12-17", "18-49", "50-64", "65+")) +
  ylab("Monthly case fatality risk (%)") + labs(color="Age group (years)") +
  theme(legend.title = element_text("Age group (years)"))

# significant drop in risks at the end of study period - likely due to lag in reporting of severe outcomes

# due to lag in reporting of severe outcomes, project august risks of hospitalization and death forward
august_rates <- summary_data_age_month %>% filter(months_since_Jan2020 == 19)
summary_data_age_month_extended <- summary_data_age_month %>% group_by(months_since_Jan2020) %>%
  mutate(hosp_rate = ifelse(months_since_Jan2020 >= 19, august_rates$hosp_rate, hosp_rate),
         death_rate = ifelse(months_since_Jan2020 >= 19, august_rates$death_rate, death_rate))

summary_data_age_vacc <- summary_data_age %>%
  left_join(summary_data_age_month_extended %>% select(months_since_Jan2020, age_hand_cut, hosp_rate, death_rate),
            c("months_since_Jan2020", "age_hand_cut"))

saveRDS(summary_data_age_vacc, "data/hosp_death_rate_week_month_rates.RDS")




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

