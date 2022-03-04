###################################################################################################
#Title: Vaccination Data Cleaning
#Author: Sophia Tan
###################################################################################################


rm(list=ls())
source("configuration.R")

#### POPULATION OF VACCINE-ELIGIBLE AGE GROUPS (12-17, 18-49, 50-64, 65+) ####
# estimates published https://covid19.ca.gov/vaccination-progress-data/#progress-by-group
pop_age <- c(3168617,17024654,7452506,6528949)

#### LOAD PUBLIC VACCINATION DATA ####
# https://data.chhs.ca.gov/dataset/vaccine-progress-dashboard/resource/faee36da-bd8c-40f7-96d4-d8f283a12b0a
chhs_vaccination_data <- read_csv("/mnt/projects/covid_partners/ucsf_lo/Data/chhs_vaccination_data.csv")
chhs_vaccination_data %>% head()

#### CLEAN VACCINATION DATA ####
  # remove other demographic variables
vacc_age <- chhs_vaccination_data %>% filter(demographic_category == "Age Group")
vacc_age <- vacc_age %>% select(demographic_value, administered_date, moderna_doses, pfizer_doses, jj_doses, at_least_one_dose, fully_vaccinated)

  # number vaccinated (at least 1 dose) by end of study period (including early vaccination, 5-11, and unknown age)
sum((vacc_age %>% filter(administered_date <= "2021-10-16"))$at_least_one_dose)

  # remove vaccinations with unknown age or in 5-11 category
  # include vaccination only through October 16, 2021
vacc_age <- vacc_age %>% filter(demographic_value != "Unknown Agegroup" & demographic_value != "5-11") %>%
  filter(administered_date <= "2021-10-16") %>%
  rename("age" = "demographic_value", "date" = "administered_date", "atleast_1_dose" = "at_least_one_dose", "full_vacc" = "fully_vaccinated")

  # total number through end of study period (including early vaccination)
sum((vacc_age %>% filter(date <= "2021-10-16"))$atleast_1_dose)
  # number excluded because vaccinated before post-vaccine period
sum((vacc_age %>% filter(date <= "2020-11-28"))$atleast_1_dose)

  # filter out early vaccination - vacc_age includes vaccination from November 29, 2020 to October 16, 2021 to match case data
vacc_age <- vacc_age %>% filter(date >= "2020-11-29")

  # total number vaccinated in vaccine era
num_vacc <- sum(vacc_age$atleast_1_dose)
num_vacc
  # percentage vaccinated (at least 1 dose) of vaccine-eligible population
num_vacc/sum(pop_age) * 100


#### CREATE FULL DATASET OF VACCINATION BY AGE OVER TIME ####
  # convert date of vaccination to weeks since January 1, 2020 to match case data
  # aggregate daily vaccination data to weekly data
jan_1 <- as.Date("2020-01-01", format = "%Y-%m-%d")
vacc_age <- vacc_age %>%
  mutate(weeks_since_Jan2020 = round(as.vector(difftime(date, jan_1, units = "weeks"))))

week_vacc <- vacc_age %>% group_by(weeks_since_Jan2020, age) %>% summarise(jj_doses = sum(jj_doses, na.rm=T),
                                                                           moderna_doses = sum(moderna_doses, na.rm=T),
                                                                           pfizer_doses = sum(pfizer_doses, na.rm=T),
                                                                           atleast_1_dose = sum(atleast_1_dose, na.rm=T),
                                                                           full_vacc = sum(full_vacc, na.rm=T))

  # calculate cumulative vaccine coverage over time in each age group
vacc <- week_vacc %>% group_by(age) %>% mutate(cum_atleast_1 = cumsum(atleast_1_dose),
                                               cum_full = cumsum(full_vacc)) %>% ungroup()
vacc <- vacc %>% group_by(weeks_since_Jan2020) %>% mutate(cum_atleast_1_perc = cum_atleast_1/pop_age*100,
                                                          cum_full_perc = cum_full/pop_age*100) %>% ungroup()

  # plot cumulative vaccine coverage over time
ggplot(vacc, aes(weeks_since_Jan2020, cum_atleast_1_perc, color=age)) + geom_line() +
  xlab("Weeks since January 2020") +
  ylab("Cumulative vaccine coverage (%)") +
  theme(legend.title = element_text("Age group"))


  # data isn't available for dates of first/second doses
  # approximate proportion of each vaccine-eligible population that received the Moderna and Pfizer/BioNTech vaccines)
  # prep for alternative model analysis
total_doses <- vacc %>% group_by(age) %>% summarise(total_moderna = sum(moderna_doses),
                                                    total_pfizer = sum(pfizer_doses))
total_doses <- total_doses %>% mutate(total = rowSums(select(., total_moderna, total_pfizer)),
                                      prop_moderna = total_moderna/total,
                                      prop_pfizer = total_pfizer/total)

  # vaccines by manufacturer (Janssen v. Pfizer/BioNTech + Moderna)
vacc$p_m_vacc <- vacc$atleast_1_dose - vacc$jj_doses
jj <- vacc %>% select(age, jj_doses, weeks_since_Jan2020) %>% spread(age, jj_doses, fill=0)
names(jj)[2:5] <- paste("jj",names(jj)[2:5])
p_m <- vacc %>% select(age, p_m_vacc, weeks_since_Jan2020) %>% spread(age, p_m_vacc, fill=0)
names(p_m)[2:5] <- paste("p_m",names(p_m)[2:5])

  # save intermediate datasets for alternative analysis
saveRDS(total_doses, "Direct Effects Analysis/Data/prop_pfizer_moderna_data.RDS")
saveRDS(jj, "Direct Effects Analysis/Data/jj_full_data.RDS")
saveRDS(p_m, "Direct Effects Analysis/Data/pfizer_moderna_full_data.RDS")


  # spread vaccine coverage data
over18 <- sum(pop_age[2:4])
vacc_18up <- vacc %>% filter(vacc$age!="12-17")
vacc_18up <- vacc_18up %>% group_by(weeks_since_Jan2020) %>% summarise(age=age, cum_atleast_1_perc=cum_atleast_1_perc,atleast_1_dose = sum(atleast_1_dose)) %>% ungroup()
vacc_18up$age <- as.factor(vacc_18up$age)
levels(vacc_18up$age) <- paste0("vacc_cum_", c("18_50", "50_65", "65"))
vacc_spread_18up <- vacc_18up %>% spread(age, cum_atleast_1_perc) %>% mutate(vacc_cum = cumsum(atleast_1_dose)/over18*100)


over12 <- sum(pop_age)
vacc <- vacc %>% group_by(weeks_since_Jan2020) %>% summarise(age=age, cum_atleast_1_perc=cum_atleast_1_perc,atleast_1_dose = sum(atleast_1_dose)) %>% ungroup()
vacc$age <- as.factor(vacc$age)
levels(vacc$age) <- paste0("vacc_cum_", c("12_18", "18_50", "50_65", "65"))
vacc_spread <- vacc %>% spread(age, cum_atleast_1_perc) %>% mutate(vacc_cum = cumsum(atleast_1_dose)/over12*100) %>%
  mutate(vacc_cum_18up = vacc_spread_18up$vacc_cum)

vacc_avg <- (vacc_spread %>% apply(2, mean))[3:8]


saveRDS(vacc_spread, "data/vaccination_coverage_data.RDS")




#### CREATE FULL DATASET CONSIDERING AGE-BASED ELIGIBILITY ####
# determined cutoffs for age-based eligibility based on vaccine rollout in CA
# 12-17 year olds became widely eligible after 16+ became eligible in mid-April
# 18-64 year olds eligible in phases due to occupational risk (used beginning of Phase 1b of vaccination)
# 65+ widely eligible in mid-January
vacc_12_17 <- filter(week_vacc, age == "12-17") %>% filter(weeks_since_Jan2020 >= 67)
vacc_18_64 <- filter(week_vacc, age == "18-49" | age=="50-64") %>% filter(weeks_since_Jan2020 >= 59)
vacc_65 <- filter(week_vacc, age == "65+") %>% filter(weeks_since_Jan2020 >= 54)
vacc_age <- rbind(vacc_12_17, vacc_18_64, vacc_65)

all_groups_weeks <- expand.grid(age = unique(vacc_age$age), weeks_since_Jan2020 = unique(vacc_age$weeks_since_Jan2020))

full_vacc_age <- vacc_age %>% merge(all_groups_weeks, all.y = TRUE) %>% replace_na(list(jj_doses = 0, atleast_1_dose = 0, full_vacc=0))

  # calculate cumulative vaccine coverage over time in each age group
vacc <- full_vacc_age %>% group_by(age) %>% mutate(cum_atleast_1 = cumsum(atleast_1_dose),
                                               cum_full = cumsum(full_vacc)) %>% ungroup()
vacc <- vacc %>% group_by(weeks_since_Jan2020) %>% mutate(cum_atleast_1_perc = cum_atleast_1/pop_age*100,
                                                          cum_full_perc = cum_full/pop_age*100) %>% ungroup()


  # data isn't available for dates of first/second doses
  # approximate proportion of each vaccine-eligible population that received the Moderna and Pfizer/BioNTech vaccines)
  # prep for alternative model analysis (age-based)
total_doses <- vacc %>% group_by(age) %>% summarise(total_moderna = sum(moderna_doses, na.rm=T),
                                                    total_pfizer = sum(pfizer_doses, na.rm=T))
total_doses <- total_doses %>% mutate(total = rowSums(select(., total_moderna, total_pfizer)),
                                      prop_moderna = total_moderna/total,
                                      prop_pfizer = total_pfizer/total)

  # vaccines by manufacturer (Janssen v. Pfizer/BioNTech + Moderna)
vacc$p_m_vacc <- vacc$atleast_1_dose - vacc$jj_doses
jj <- vacc %>% select(age, jj_doses, weeks_since_Jan2020) %>% spread(age, jj_doses, fill=0)
names(jj)[2:5] <- paste("jj",names(jj)[2:5])
p_m <- vacc %>% select(age, p_m_vacc, weeks_since_Jan2020) %>% spread(age, p_m_vacc, fill=0)
names(p_m)[2:5] <- paste("p_m",names(p_m)[2:5])

  # save intermediate datasets for alternative analysis (age-based)
saveRDS(total_doses, "data/prop_pfizer_moderna_data_age_based.RDS")
saveRDS(jj, "data/jj_full_data_age_based.RDS")
saveRDS(p_m, "data/pfizer_moderna_full_data_age_based.RDS")

