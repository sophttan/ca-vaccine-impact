###################################################################################################
#Title: Direct Effects of Vaccination in CA Alternative Analysis
# Results
#Author: Sophia Tan
###################################################################################################
setwd("/mnt/projects/covid_partners/ucsf_lo")

source("Direct Effects Analysis/Main Scripts/final-scripts-122021/alternative-model-analysis-functions.R")

hosp_data <- readRDS("Direct Effects Analysis/Hospitalizations and deaths/hosp_rate_week_month_rates.RDS") %>% filter(weeks_since_Jan2020 >=48)
vacc_spread <- readRDS("Direct Effects Analysis/Data/vaccination_coverage_data.RDS")
res_12_18 <- readRDS("Direct Effects Analysis/final results/res_12_18_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("Direct Effects Analysis/final results/res_18_50_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("Direct Effects Analysis/final results/res_50_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("Direct Effects Analysis/final results/res_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 48)

prep_res <- function(data, age) {
  res <- data[,1:3]-data[,4]
  
  res <- as.data.frame(cbind(res, data))
  names(res) <- c("pred_averted", "lb_averted", "ub_averted",
                  "pred", "lb", "ub", "actual_hosp")
  
  res$age <- age
  
  res <- res %>% mutate(pred_averted = ifelse(pred_averted < 0, 0, pred_averted),
                        lb_averted = ifelse(lb_averted < 0, 0, lb_averted),
                        ub_averted = ifelse(ub_averted < 0, 0, ub_averted),
                        pred = ifelse(pred < actual_hosp, actual_hosp, pred),
                        lb = ifelse(lb < actual_hosp, actual_hosp, lb),
                        ub = ifelse(ub < actual_hosp, actual_hosp, ub))
  
  res
}

groups<-hosp_data[1:5,]$age_hand_cut[2:5]
results <- c("res_12_18", "res_18_50", "res_50_65", "res_65")

total_res_cases <- NULL
tbl_cases <- NULL
red_cases <- NULL

total_res_hosp <- NULL
tbl_hosp <- NULL
red_hosp <- NULL

total_res_death <- NULL
tbl_death <- NULL
red_death <- NULL

for (i in 1:4) {
  res <- get(results[i])
  group <- groups[i]
  subset <- hosp_data %>% filter(age_hand_cut==group)
  
  if (i == 1) {
    res <- filter(res, weeks_since_Jan2020 >= 67)    
    subset <- filter(subset, weeks_since_Jan2020 >= 67)
  } else if (i == 2) {
    res <- filter(res, weeks_since_Jan2020 >= 59)   
    subset <- filter(subset, weeks_since_Jan2020 >= 59)
  } else if (i == 3) {
    res <- filter(res, weeks_since_Jan2020 >= 59)   
    subset <- filter(subset, weeks_since_Jan2020 >= 59)
  } else {
    res <- filter(res, weeks_since_Jan2020 >= 54)  
    subset <- filter(subset, weeks_since_Jan2020 >= 54)
  }

  hosp <- (subset$hosp_rate/100) * res[,2:4]
  death <- (subset$death_rate/100) * res[,2:4]
  hosp$obs <- subset$num_hosp
  death$obs <- subset$num_died
  res$obs <- subset$cases
  
  hosp_res <- prep_res(hosp, group)
  death_res <- prep_res(death, group)
  cases_res <- prep_res(res %>% select(!weeks_since_Jan2020), group)
  
  averted <- apply(cases_res[,1:7], 2, sum)
  tbl_cases <- rbind(tbl_cases, averted[c(7, 4:6, 1:3)])
  total_res_cases <- rbind(total_res_cases, cases_res)
  red_cases <- rbind(red_cases, averted[1:3]/apply(res[,2:4], 2, sum)*100)
  
  averted <- apply(hosp_res[,1:7], 2, sum)
  tbl_hosp <- rbind(tbl_hosp, averted[c(7, 4:6, 1:3)])
  total_res_hosp <- rbind(total_res_hosp, hosp_res)
  red_hosp <- rbind(red_hosp, averted[1:3]/apply(hosp[,1:3], 2, sum)*100)
  
  averted <- apply(death_res[,1:7], 2, sum)
  tbl_death <- rbind(tbl_death, averted[c(7, 4:6, 1:3)])
  total_res_death <- rbind(total_res_death, death_res)
  red_death <- rbind(red_death, averted[1:3]/apply(death[,1:3], 2, sum)*100)
  
}


tbl_cases <- rbind(tbl_cases, apply(tbl_cases, 2, sum))
tbl_hosp <- rbind(tbl_hosp[2:4,], apply(tbl_hosp[2:4,], 2, sum))
tbl_death <- rbind(tbl_death[2:4,], apply(tbl_death[2:4,], 2, sum))

cases <- format(round(tbl_cases, -1), big.mark=",", trim = T)
hosp <- format(round(tbl_hosp, -1), big.mark=",", trim = T)
death <- format(round(tbl_death, -1), big.mark=",", trim = T)

cases <- as.data.frame(cbind(c(groups%>% as.character(), "Total"), paste0(cases[,2], " (", cases[,3], ", ", cases[,4], ")"),
                             paste0(cases[,5], " (", cases[,6], ", ", cases[,7], ")")))
hosp <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(hosp[,2], " (", hosp[,3], ", ", hosp[,4], ")"),
                            paste0(hosp[,5], " (", hosp[,6], ", ", hosp[,7], ")")))
death <- as.data.frame(cbind(c(groups[2:4]%>% as.character(), "Total"), paste0(death[,2], " (", death[,3], ", ", death[,4], ")"),
                             paste0(death[,5], " (", death[,6], ", ", death[,7], ")")))

tbl <- rbind(cases, hosp, death)

names(tbl) <- c("Age group", "Predicted outcome (95% UI)", "Averted outcome (95% UI)")

vacc_spread_54 <- (filter(vacc_spread, weeks_since_Jan2020 >= 54) %>% apply(2, mean))[3:8]
vacc_spread_59 <- (filter(vacc_spread, weeks_since_Jan2020 >= 59) %>% apply(2, mean))[3:8]
vacc_spread_67 <- (filter(vacc_spread, weeks_since_Jan2020 >= 67) %>% apply(2, mean))[3:8]

vacc_avg <- c(vacc_spread_67[1], vacc_spread_59[2:3], vacc_spread_54[4:6])
vacc_coverage <- vacc_avg[c(1, 2, 3, 4, 5)]
red_cases <- rbind(red_cases, (tbl_cases[5,5:7]/tbl_cases[5,2:4])*100)
red_cases <- cbind(red_cases, red_cases/vacc_coverage*100)
red_cases[red_cases > 100] <- 100

vacc_coverage <- vacc_avg[c(2, 3, 4, 6)]
red_hosp <- rbind(red_hosp[2:4,], (tbl_hosp[4,5:7]/tbl_hosp[4,2:4])*100)
red_hosp <- cbind(red_hosp, red_hosp/vacc_coverage*100)
red_hosp[red_hosp > 100] <- 100

red_death <- rbind(red_death[2:4,], (tbl_death[4,5:7]/tbl_death[4,2:4])*100)
red_death <- cbind(red_death, red_death/vacc_coverage*100)
red_death[red_death > 100] <- 100

cases <- format(round(red_cases), big.mark=",", trim=T)
hosp <- format(round(red_hosp), big.mark=",", trim=T)
death <- format(round(red_death), big.mark=",", trim=T)

cases <- cbind(paste0(cases[,1], " (", cases[,2], ", ", cases[,3], ")"),
               paste0(cases[,4], " (", cases[,5], ", ", cases[,6], ")"))
hosp <- cbind(paste0(hosp[,1], " (", hosp[,2], ", ", hosp[,3], ")"),
              paste0(hosp[,4], " (", hosp[,5], ", ", hosp[,6], ")"))
death <- cbind(paste0(death[,1], " (", death[,2], ", ", death[,3], ")"),
               paste0(death[,4], " (", death[,5], ", ", death[,6], ")"))
tbl <- cbind(tbl, rbind(cases,hosp,death))
tbl <- tbl[,c(1, 3:5)]
names(tbl) <- c("Age group", 
                "Averted outcome (95% PI)",
                "Relative reduction in outcome (%) (95% PI) Unadjusted",
                "Adjusted")
tbl %>% write_csv("Direct Effects Analysis/final tables/alternative-model-tbl-age-based.csv")
