###################################################################################################
#Title: Tables for alternative analysis results
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
setwd(here::here())

# main table
cases <- read_csv("data/ca_case_data.csv")
vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")
res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)

cases_time <- cases %>% filter(weeks_since_Jan2020 >= 48)
res_12_18$obs <- cases_time$`[12,18)`
res_18_50$obs <- cases_time$`[18,50)`
res_50_65$obs <- cases_time$`[50,65)`
res_65$obs <- cases_time$`[65,Inf)`

total_cases <- c(0, 0, 0)
total_averted <- c(0, 0, 0)
tbl <- data.frame(matrix(ncol=3))

format_res <- function(v) {
  paste0(v[1], " (", v[2], ", ", v[3], ")")
}

for (res in c("res_12_18", "res_18_50", "res_50_65", "res_65")) {
  res <- get(res)
  cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
  averted <- c(difference_cases(res, "obs", "mean"),
                     difference_cases(res, "obs", "lb"),
                     difference_cases(res, "obs", "ub"))
  reduction <- averted/cases*100

  total_cases <- total_cases + cases
  total_averted <- total_averted + averted

  cases <- format(round(cases, -1), big.mark=",")
  averted <- format(round(averted, -1), big.mark=",")
  reduction <- round(reduction)

  tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction)))
}

reduction <- total_averted/total_cases*100
cases <- format(round(total_cases, -2), big.mark=",")
averted <- format(round(total_averted, -2), big.mark=",")
reduction <- round(reduction)

tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction))
names(tbl) <- c("Predicted COVID-19 cases (95% UI)", "Averted cases COVID-19 (95% UI)", "Relative reduction in % (95% UI")
tbl[["Age group (years)"]] <- c("Total 12+", "12-17", "18-49", "50-64", "65+")
tbl <- select(tbl, 4, 1, 2, 3)

tbl %>% write_csv("tables/tables/alternative-model-tbl.csv")

rbind(res_12_18, res_18_50, res_50_65, res_65) %>% filter(weeks_since_Jan2020 >= 74) %>% apply(2, sum)



# age-based table
res_12_18 <- readRDS("results/alternative/age-based/res_12_18_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 67)
res_18_50 <- readRDS("results/alternative/age-based/res_18_50_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 59)
res_50_65 <- readRDS("results/alternative/age-based/res_50_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 59)
res_65 <- readRDS("results/alternative/age-based/res_65_sim_age.RDS")%>% filter(weeks_since_Jan2020 >= 54)
cases <- read_csv("data/ca_case_data.csv")

res_12_18$obs <- (cases %>% filter(weeks_since_Jan2020 >= 67))$`[12,18)`
res_18_50$obs <- (cases %>% filter(weeks_since_Jan2020 >= 59))$`[18,50)`
res_50_65$obs <- (cases %>% filter(weeks_since_Jan2020 >= 59))$`[50,65)`
res_65$obs <- (cases %>% filter(weeks_since_Jan2020 >= 54))$`[65,Inf)`

total_cases <- c(0, 0, 0)
total_averted <- c(0, 0, 0)
tbl <- data.frame(matrix(ncol=3))

for (res in c("res_12_18", "res_18_50", "res_50_65", "res_65")) {
  res <- get(res)
  cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
  averted <- c(difference_cases(res, "obs", "mean"),
               difference_cases(res, "obs", "lb"),
               difference_cases(res, "obs", "ub"))
  reduction <- averted/cases*100

  total_cases <- total_cases + cases
  total_averted <- total_averted + averted

  cases <- format(round(cases, -1), big.mark=",")
  averted <- format(round(averted, -1), big.mark=",")
  reduction <- round(reduction)

  tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction)))
}

reduction <- total_averted/total_cases*100
cases <- format(round(total_cases, -1), big.mark=",")
averted <- format(round(total_averted, -1), big.mark=",")
reduction <- round(reduction)

tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction))
names(tbl) <- c("Predicted COVID-19 cases (95% UI)", "Averted cases COVID-19 (95% UI)", "Relative reduction in % (95% UI")
tbl[["Age group (years)"]] <- c("Total 12+", "12-17", "18-49", "50-64", "65+")
tbl <- select(tbl, 4, 1, 2, 3)

tbl %>% write_csv("tables/tables/alternative-model-tbl-age-based.csv")




# results for analysis accounting for delta variant vaccine effectiveness
res_12_18 <- readRDS("results/alternative/delta/res_12_18_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("results/alternative/delta/res_18_50_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("results/alternative/delta/res_50_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("results/alternative/delta/res_65_sim_delta.RDS")%>% filter(weeks_since_Jan2020 >= 48)
cases <- read_csv("data/ca_case_data.csv") %>% filter(weeks_since_Jan2020 >= 48)

res_12_18$obs <- cases$`[12,18)`
res_18_50$obs <- cases$`[18,50)`
res_50_65$obs <- cases$`[50,65)`
res_65$obs <- cases$`[65,Inf)`

total_cases <- c(0, 0, 0)
total_averted <- c(0, 0, 0)
tbl <- data.frame(matrix(ncol=3))

for (res in c("res_12_18", "res_18_50", "res_50_65", "res_65")) {
  res <- get(res)
  cases <- c(sum(res$mean), sum(res$lb), sum(res$ub))
  averted <- c(difference_cases(res, "obs", "mean"),
               difference_cases(res, "obs", "lb"),
               difference_cases(res, "obs", "ub"))
  reduction <- averted/cases*100

  total_cases <- total_cases + cases
  total_averted <- total_averted + averted

  cases <- format(round(cases, -1), big.mark=",")
  averted <- format(round(averted, -1), big.mark=",")
  reduction <- round(reduction)

  tbl <- rbind(tbl, list(format_res(cases), format_res(averted), format_res(reduction)))
}

reduction <- total_averted/total_cases*100
cases <- format(round(total_cases, -1), big.mark=",")
averted <- format(round(total_averted, -1), big.mark=",")
reduction <- round(reduction)

tbl[1,] <- list(format_res(cases), format_res(averted), format_res(reduction))
names(tbl) <- c("Predicted COVID-19 cases (95% UI)", "Averted cases COVID-19 (95% UI)", "Relative reduction in % (95% UI")
tbl[["Age group (years)"]] <- c("Total 12+", "12-17", "18-49", "50-64", "65+")
tbl <- select(tbl, 4, 1, 2, 3)

tbl %>% write_csv("tables/tables/alternative-model-tbl-delta.csv")

rbind(res_12_18, res_18_50, res_50_65, res_65) %>% filter(weeks_since_Jan2020 >= 74) %>% apply(2, sum)
