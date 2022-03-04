###################################################################################################
#Title: Hospitalizations and deaths
# Sensitivity analysis varying time definition of beginning of vaccine era
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("src/3-hospitalizations and deaths/hosp_and_death_functions.R")

res <- readRDS("results/primary/primary-model-vacc-threshold.RDS")
rates <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >= 48)

total_res_hosp <- NULL
total_res_death <- NULL

for (i in res$vaccine%>%unique()) {
  res_sub <- res %>% filter(vaccine==i)
  rates_sub <- rates %>% filter(weeks_since_Jan2020 >= i)

  groups <- (rates$age_hand_cut %>% unique())[3:5]
  for (group in 1:3) {
    subset <- rates_sub %>% filter(age_hand_cut==groups[group])
    if (group==1){
      subset_cases <- res_sub %>% select(pred_18_50, lb_18_50, ub_18_50)
    } else if (group==2){
      subset_cases <- res_sub %>% select( pred_50_65, lb_50_65, ub_50_65)
    } else {
      subset_cases <- res_sub %>% select(pred_65, lb_65, ub_65)
    }

    hosp <- subset_cases * subset$hosp_rate/100
    death <- subset_cases * subset$death_rate/100

    hosp_res <- prep_res(cbind(subset$num_hosp, hosp), groups[group]) %>% mutate(vaccine=i)
    death_res <- prep_res(cbind(subset$num_died, death), groups[group]) %>% mutate(vaccine=i)

    total_res_hosp <- rbind(total_res_hosp, hosp_res)
    total_res_death <- rbind(total_res_death, death_res)
  }
}


saveRDS(total_res_death, "results/hospitalizations and deaths/death_results_primary_model_vacc_threshold.RDS")
saveRDS(total_res_hosp, "results/hospitalizations and deaths/hosp_results_primary_model_vacc_threshold.RDS")
