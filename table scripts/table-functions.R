###################################################################################################
#Title: Functions for tables
#Author: Sophia Tan
###################################################################################################


format_res <- function(v) {
  paste0(v[1], " (", v[2], ", ", v[3], ")")
}

make_cases_table_p <- function(d, vacc_avg, include_prediction, type) {
  tbl <- matrix(ncol=5)
  total_obs <- 0
  total_pred <- c(0, 0, 0)
  total_averted <- c(0, 0, 0)
  for (group in 1:4) {
    if (group==1){
      subset_cases <- d %>% select(`[12,18)`, pred_12_18, lb_12_18, ub_12_18)
    } else if (group==2){
      subset_cases <- d %>% select(`[18,50)`, pred_18_50, lb_18_50, ub_18_50)
    } else if (group==3){
      subset_cases <- d %>% select(`[50,65)`, pred_50_65, lb_50_65, ub_50_65)
    } else{
      subset_cases <- d %>% select(`[65,Inf)`, pred_65, lb_65, ub_65)
    }

    subset_cases <- apply(subset_cases, 2, sum)
    obs <- subset_cases[1]
    pred <- subset_cases[2:4]
    averted <- pred-obs

    total_obs <- total_obs+obs
    total_pred <- total_pred + pred
    total_averted <- total_averted + averted

    reduction <- averted/pred*100
    adj_red <- (reduction/vacc_avg[group])*100
    adj_red[adj_red > 100] <- 100

    obs <- format(obs, big.mark=",", trim=T)
    pred <- format(round(pred, -1), big.mark=",", trim = T)
    averted <- format(round(averted, -1), big.mark=",", trim = T)
    reduction <- format(round(reduction))
    adj_red <- format(round(adj_red))

    tbl <- rbind(tbl,
                 c(obs, format_res(pred), format_res(averted), format_res(reduction), format_res(adj_red)))

  }

  reduction <- total_averted/total_pred*100
  adj_red <- (reduction/vacc_avg[group+1])*100
  adj_red[adj_red > 100] <- 100

  total_obs <- format(total_obs, big.mark=",", trim=T)
  total_pred <- format(round(total_pred, -1), big.mark=",", trim = T)
  total_averted <- format(round(total_averted, -1), big.mark=",", trim = T)
  reduction <- format(round(reduction))
  adj_red <- format(round(adj_red))

  tbl[1,] <- c(total_obs, format_res(total_pred), format_res(total_averted), format_res(reduction), format_res(adj_red))
  tbl <- tbl %>% as.data.frame()
  tbl$age <- c("12+", "12-17", "18-49", "50-64", "65+")

  tbl <- tbl %>% select(6, 1, 2, 3, 4, 5)

  if (type=="p") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% PI)",
                    "Averted COVID-19 outcome (95% PI)", "Relative reduction in outcome (%) (95% PI) Unadjusted", "Adjusted")
  } else if (type=="a") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% UI)",
                    "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI) Unadjusted", "Adjusted")

  } else {
    names(tbl) <- c("Age group (years)", "Observed outcome", "Predicted outcome (95% PI or UI)",
                    "Averted outcome (95% PI or UI)", "Relative reduction (%) (95% PI or UI) Unadjusted", "Adjusted")

  }

  if (!include_prediction) {
    tbl <- tbl %>% select(1, 4, 5, 6)
  }
  tbl
}



make_cases_table_a <- function(vacc_avg, include_prediction, type) {
  tbl <- matrix(ncol=5)
  total_obs <- 0
  total_pred <- c(0, 0, 0)
  total_averted <- c(0, 0, 0)

  res <- c("res_12_18", "res_18_50", "res_50_65", "res_65")
  for (group in 1:4) {
    subset_cases <- get(res[group])

    subset_cases <- apply(subset_cases, 2, sum)
    obs <- subset_cases[5]
    pred <- subset_cases[2:4]
    averted <- pred-obs

    total_obs <- total_obs+obs
    total_pred <- total_pred + pred
    total_averted <- total_averted + averted

    reduction <- averted/pred*100
    adj_red <- (reduction/vacc_avg[group])*100
    adj_red[adj_red > 100] <- 100

    obs <- format(obs, big.mark=",", trim=T)
    pred <- format(round(pred, -1), big.mark=",", trim = T)
    averted <- format(round(averted, -1), big.mark=",", trim = T)
    reduction <- format(round(reduction))
    adj_red <- format(round(adj_red))

    tbl <- rbind(tbl,
                 c(obs, format_res(pred), format_res(averted), format_res(reduction), format_res(adj_red)))

  }

  reduction <- total_averted/total_pred*100
  adj_red <- (reduction/vacc_avg[group+1])*100
  adj_red[adj_red > 100] <- 100

  total_obs <- format(total_obs, big.mark=",", trim=T)
  total_pred <- format(round(total_pred, -1), big.mark=",", trim = T)
  total_averted <- format(round(total_averted, -1), big.mark=",", trim = T)
  reduction <- format(round(reduction))
  adj_red <- format(round(adj_red))

  tbl[1,] <- c(total_obs, format_res(total_pred), format_res(total_averted), format_res(reduction), format_res(adj_red))
  tbl <- tbl %>% as.data.frame()
  tbl$age <- c("12+", "12-17", "18-49", "50-64", "65+")

  tbl <- tbl %>% select(6, 1, 2, 3, 4, 5)

  if (type=="p") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% PI)",
                    "Averted COVID-19 outcome (95% PI)", "Relative reduction in outcome (%) (95% PI) Unadjusted", "Adjusted")
  } else if (type=="a") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% UI)",
                    "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI) Unadjusted", "Adjusted")

  } else {
    names(tbl) <- c("Age group (years)", "Observed outcome", "Predicted outcome (95% PI or UI)",
                    "Averted outcome (95% PI or UI)", "Relative reduction (%) (95% PI or UI) Unadjusted", "Adjusted")

  }

  if (!include_prediction) {
    tbl <- tbl %>% select(1, 4, 5, 6)
  }
  tbl
}



make_hosp_death_tbl <- function(d, vacc_avg, include_prediction, type) {

  format_res <- function(v) {
    paste0(v[,1], " (", v[,2], ", ", v[,3], ")")
  }

  d <- d %>% group_by(age) %>% summarise_all(sum) %>% ungroup() %>% as.data.frame()

  d <- rbind(c(NA, d%>%select(!1)%>%apply(2,sum)), d)

  reduction <- d[,2:4]/d[,5:7]*100
  adj_red <- (reduction/vacc_avg)*100
  adj_red[adj_red > 100] <- 100

  obs <- format(d[,8], big.mark=",", trim=T)
  pred <- format(round(d[,5:7], -1), big.mark=",", trim = T)
  averted <- format(round(d[,2:4], -1), big.mark=",", trim = T)
  reduction <- format(round(reduction))
  adj_red <- format(round(adj_red))

  tbl <- d %>% select(age)
  tbl <- tbl %>% mutate(age=c("18+", "18-49", "50-64", "65+"))

  tbl <- cbind(tbl, obs, format_res(pred), format_res(averted),
               format_res(reduction), format_res(adj_red))

  if (type=="p") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% PI)",
                    "Averted COVID-19 outcome (95% PI)", "Relative reduction in outcome (%) (95% PI) Unadjusted", "Adjusted")
  } else if (type=="a") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% UI)",
                    "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI) Unadjusted", "Adjusted")

  } else {
    names(tbl) <- c("Age group (years)", "Observed outcome", "Predicted outcome (95% PI or UI)",
                    "Averted outcome (95% PI or UI)", "Relative reduction (%) (95% PI or UI) Unadjusted", "Adjusted")

  }

  if (!include_prediction) {
    tbl <- tbl %>% select(1, 4, 5, 6)
  }
  tbl
}


make_cases_table_p_age <- function(d, vacc_avg, include_prediction, type) {
  tbl <- matrix(ncol=5)
  total_obs <- 0
  total_pred <- c(0, 0, 0)
  total_averted <- c(0, 0, 0)
  for (group in 1:4) {
    if (group==1){
      subset_cases <- d %>% filter(weeks_since_Jan2020 >= 67)%>% select(`[12,18)`,pred_12_18, lb_12_18, ub_12_18)
    } else if (group==2){
      subset_cases <- d %>% filter(weeks_since_Jan2020 >= 59) %>% select(`[18,50)`,pred_18_50, lb_18_50, ub_18_50)
    } else if (group==3) {
      subset_cases <- d %>% filter(weeks_since_Jan2020 >= 59)%>% select(`[50,65)`,pred_50_65, lb_50_65, ub_50_65)
    } else{
      subset_cases <- d %>% filter(weeks_since_Jan2020 >= 54)%>% select(`[65,Inf)`,pred_65, lb_65, ub_65)
    }

    subset_cases <- apply(subset_cases, 2, sum)
    obs <- subset_cases[1]
    pred <- subset_cases[2:4]
    averted <- pred-obs

    total_obs <- total_obs+obs
    total_pred <- total_pred + pred
    total_averted <- total_averted + averted

    reduction <- averted/pred*100
    adj_red <- (reduction/vacc_avg[group])*100
    adj_red[adj_red > 100] <- 100

    obs <- format(obs, big.mark=",", trim=T)
    pred <- format(round(pred, -1), big.mark=",", trim = T)
    averted <- format(round(averted, -1), big.mark=",", trim = T)
    reduction <- format(round(reduction))
    adj_red <- format(round(adj_red))

    tbl <- rbind(tbl,
                 c(obs, format_res(pred), format_res(averted), format_res(reduction), format_res(adj_red)))

  }

  reduction <- total_averted/total_pred*100
  adj_red <- (reduction/vacc_avg[group+1])*100
  adj_red[adj_red > 100] <- 100

  total_obs <- format(total_obs, big.mark=",", trim=T)
  total_pred <- format(round(total_pred, -1), big.mark=",", trim = T)
  total_averted <- format(round(total_averted, -1), big.mark=",", trim = T)
  reduction <- format(round(reduction))
  adj_red <- format(round(adj_red))

  tbl[1,] <- c(total_obs, format_res(total_pred), format_res(total_averted), format_res(reduction), format_res(adj_red))
  tbl <- tbl %>% as.data.frame()
  tbl$age <- c("12+", "12-17", "18-49", "50-64", "65+")

  tbl <- tbl %>% select(6, 1, 2, 3, 4, 5)

  if (type=="p") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% PI)",
                    "Averted COVID-19 outcome (95% PI)", "Relative reduction in outcome (%) (95% PI) Unadjusted", "Adjusted")
  } else if (type=="a") {
    names(tbl) <- c("Age group (years)", "Observed COVID-19 outcome", "Predicted COVID-19 outcome (95% UI)",
                    "Averted COVID-19 outcome (95% UI)", "Relative reduction in outcome (%) (95% UI) Unadjusted", "Adjusted")

  } else {
    names(tbl) <- c("Age group (years)", "Observed outcome", "Predicted outcome (95% PI or UI)",
                    "Averted outcome (95% PI or UI)", "Relative reduction (%) (95% PI or UI) Unadjusted", "Adjusted")

  }

  if (!include_prediction) {
    tbl <- tbl %>% select(1, 4, 5, 6)
  }
  tbl
}



