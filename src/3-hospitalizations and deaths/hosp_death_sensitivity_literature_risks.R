###################################################################################################
#Title: Hospitalization and deaths sensitivity analysis using literature estimates of hospitalization and death risk
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("src/3-hospitalizations and deaths/hosp_and_death_functions.R")
primary_cases <- readRDS("results/primary/primary-model-main-results.RDS")

#hospitalization and death risks estimated from data available in table
#use age-based estimates of cases, hospitalizations, and deaths in the not fully vaccinated population
#https://www.cdc.gov/mmwr/volumes/70/wr/mm7037e1.htm?s_cid=mm7037e1_w
hosp_rate <- c(10526/331151, 9158/93474, 9199/42884)
death_rate <- c(609/331151, 1380/93474, 3137/42884)

total_res_hosp <- NULL
total_res_death <- NULL

rates <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >= 48)
groups <- c("[18,50)", "[50,65)", "[65,Inf)")
for (group in 1:3) {
  subset <- rates %>% filter(age_hand_cut==groups[group])
  if (group==1){
    subset_cases <- primary_cases %>% select(pred_18_50, lb_18_50, ub_18_50)
  } else if (group==2){
    subset_cases <- primary_cases %>% select( pred_50_65, lb_50_65, ub_50_65)
  } else{
    subset_cases <- primary_cases %>% select(pred_65, lb_65, ub_65)
  }

  hosp <- subset_cases * hosp_rate[group]
  death <- subset_cases * death_rate[group]

  hosp_res <- prep_res(cbind(hosp,subset$num_hosp), groups[group])
  death_res <- prep_res(cbind(death,subset$num_died), groups[group])

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)
}

saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_primary_model_lit_risk.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_primary_model_lit_risk.RDS")




res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS") %>% filter(weeks_since_Jan2020>=48)


results <- c("res_18_50", "res_50_65", "res_65")
total_res_hosp <- NULL
total_res_death <- NULL

for (i in 1:3) {
  res <- get(results[i])
  group <- groups[i]

  subset <- rates %>% filter(age_hand_cut==group)

  hosp <-  hosp_rates[i]*res[,2:4]
  death <- death_rates[i]*res[,2:4]
  hosp$obs <- subset$num_hosp
  death$obs <- subset$num_died

  hosp_res <- prep_res(hosp, group)
  death_res <- prep_res(death, group)

  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  total_res_death <- rbind(total_res_death, death_res)

}


saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_alternative_model_lit_risk.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_alternative_model_lit_risk.RDS")


