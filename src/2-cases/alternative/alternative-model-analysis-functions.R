###################################################################################################
#Title: Functions for estimating averted cases in CA using susceptibility-based model
#Author: Sophia Tan
###################################################################################################


# make table that has vaccine effectiveness over time by manufacturer
# option to make table with effectivness against the delta variant
make_vacc_table <- function(row, delta) {
  d <- data.frame(weeks = seq(0, 94))
  if (delta==T) {
    d$jj <- row$jj_delta
    d$p1 <- row$p1_delta
    d$p2 <- ifelse(d$weeks < avg_time_pfizer, row$p1_delta,
                   ifelse(d$weeks >= 120/7 + avg_time_pfizer, row$p_wane_delta, row$p2_delta))
    d$m1 <- row$m1_delta
    d$m2 <- ifelse(d$weeks < avg_time_moderna, row$m1_delta,
                   ifelse(d$weeks >= 120/7 + avg_time_moderna, row$m_wane_delta, row$m2_delta))
  } else {
    d$jj <- row$jj
    d$p1 <- row$p1
    d$p2 <- ifelse(d$weeks < avg_time_pfizer, row$p1,
                   ifelse(d$weeks >= 120/7 + avg_time_pfizer, row$p_wane, row$p2))
    d$m1 <- row$m1
    d$m2 <- ifelse(d$weeks < avg_time_moderna, row$m1,
                   ifelse(d$weeks >= 120/7 + avg_time_moderna, row$m_wane, row$m2))
  }
  d
}


# estimate total infections by age group and combine with vaccine data
prep_inf_data <- function(prop_symp, data) {

  d <- data %>% group_by(weeks_since_Jan2020) %>%
    summarise(age_hand_cut_inf = age_hand_cut_inf, total_inf_age = cases/prop_symp) %>%
    spread(age_hand_cut_inf, total_inf_age, 0)

  d$total_inf_under12 <- d$`[0,12)`
  d$total_inf_over12 <- rowSums(d[,3:8])
  d$total_inf_12_18 <- rowSums(d[,3])
  d$total_inf_18_50 <- rowSums(d[,4:5])
  d$total_inf_50_65 <- rowSums(d[,6:7])
  d$total_inf_65 <- rowSums(d[,8])
  d$total_inf <- rowSums(d[,2:8])

  d
}

# prep vaccination data for each vaccine-eligible age group
prep_vacc_data <- function(group, vacc_data) {

  if (group==1) {
    subset <- vacc_data %>% select(weeks_since_Jan2020, `jj 12-17`, `p_m 12-17`)
  } else if (group==2) {
    subset <- vacc_data %>% select(weeks_since_Jan2020, `jj 18-49`, `p_m 18-49`)
  } else if (group==3) {
    subset <- vacc_data %>% select(weeks_since_Jan2020, `jj 50-64`, `p_m 50-64`)
  } else {
    subset <- vacc_data %>% select(weeks_since_Jan2020, `jj 65+`, `p_m 65+`)
  }

  names(subset)[2] <- 'jj'
  p_m_subset <- subset[,3]
  p_pfizer <- total_doses[group,]$prop_pfizer
  p_moderna <- total_doses[group,]$prop_moderna
  subset <- subset %>% select(!3) %>%
    mutate(p1 = p_m_subset*p_pfizer*p_no_second_pfizer,
           p2 = p_m_subset*p_pfizer*p_second_pfizer,
           m1 = p_m_subset*p_moderna*p_no_second_moderna,
           m2 = p_m_subset*p_moderna*p_second_moderna)

  vacc <- rowSums(subset[,2:6], na.rm=T)
  subset$vacc <- vacc
  subset$cum_vacc <- cumsum(vacc)

  subset <- subset %>% mutate(jj_perc = jj/vacc, p1_perc = p1/vacc, p2_perc = p2/vacc,
                              m1_perc = m1/vacc, m2_perc = m2/vacc)

  subset
}


# calculate the number of new vaccinations each week among the susceptible population
# since we don't want to double count individuals who have previously been infected and get vaccinated
calculate_new_vacc <- function(vacc_adjusted, prev_inf, vacc_inf, cum_vacc, pop) {
  # vacc_adjusted - new vaccinations in a week
  # prev_inf - total number of people who have been infected
  # vacc_inf - number of people who have been infected and vaccinated already
  # cum_vacc - total number of people who have been vaccinated
  prev_inf_adj <- prev_inf - vacc_inf
  pop_adj <- pop - cum_vacc
  vaccinated_and_infected <- prev_inf_adj/pop_adj
  vacc_adjusted - vacc_adjusted * vaccinated_and_infected
}


# calculate susceptible population over time
calculate_vacc <- function(group, d, delta=F) {
  data <- d

  if (group==1) {
    data$cum_inf <- cumsum(data$total_inf_12_18)
    data$prev_inf <- c(0, data$cum_inf[1:nrow(data)-1])
    data <- left_join(data, vacc_12_17, by="weeks_since_Jan2020")
  } else if (group==2) {
    data$cum_inf <- cumsum(data$total_inf_18_50)
    data$prev_inf <- c(0, data$cum_inf[1:nrow(data)-1])
    data <- left_join(data, vacc_18_49, by="weeks_since_Jan2020")
  } else if (group==3) {
    data$cum_inf <- cumsum(data$total_inf_50_65)
    data$prev_inf <- c(0, data$cum_inf[1:nrow(data)-1])
    data <- left_join(data, vacc_50_64, by="weeks_since_Jan2020")
  } else {
    data$cum_inf <- cumsum(data$total_inf_65)
    data$prev_inf <- c(0, data$cum_inf[1:nrow(data)-1])
    data <- left_join(data, vacc_65, by="weeks_since_Jan2020")
  }

  vacc_inf <- 0
  new_vaccs <- NULL
  vaccination_begins <- filter(data, vacc > 0)$weeks_since_Jan2020[1]

  for (j in (vaccination_begins+1):nrow(data)){
    row <- data[j,]
    new <- calculate_new_vacc(row$vacc, row$prev_inf, vacc_inf, row$cum_vacc, pop_age[group])
    vacc_inf <- vacc_inf + row$vacc-new
    new_vaccs <- c(new_vaccs, new)
  }

  data$new_vacc <- c(rep(0, vaccination_begins), new_vaccs)
  data$cum_new_vacc <- cumsum(data$new_vacc)

  data <- data %>% mutate(jj = jj_perc*new_vacc, p1 = p1_perc*new_vacc, p2 = p2_perc*new_vacc,
                        m1 = m1_perc*new_vacc, m2 = m2_perc*new_vacc)

  data$full_immune_vacc <- 0

  for (v in c("jj", "p1", "p2", "m1", "m2")){
    effectiveness <- protected[[v]]
    effectiveness_delta <- protected_delta[[v]]
    for (k in vaccination_begins:(nrow(data)-1)) {
      immune <- filter(data, weeks_since_Jan2020==k)[[v]]
      if(is.na(immune)) {
        data$full_immune_vacc <- data$full_immune_vacc
      } else {
        if (delta==T){
          if (k < 74) {
            effectiveness_sub <- effectiveness[1:(74-k)]
            effectiveness_sub <- c(effectiveness_sub, effectiveness_delta[(74-k+1):(94-k)])
            data$full_immune_vacc <- data$full_immune_vacc + c(rep(0, k), immune*effectiveness_sub)
          } else {
            effectiveness_sub <- effectiveness_delta[1:(94-k)]
            data$full_immune_vacc <- data$full_immune_vacc + c(rep(0, k), immune*effectiveness_sub)
          }
        } else {
          data$full_immune_vacc <- data$full_immune_vacc + c(rep(0, k), immune*effectiveness[1:(nrow(data)-k)])
        }
      }
    }
  }

  susceptible <- pop_age[group] - (data$full_immune_vacc + data$prev_inf)

  if (group==1) {
    d$susceptible_12_18 <- susceptible
  } else if (group==2) {
    d$susceptible_18_50 <- susceptible
  } else if (group==3) {
    d$susceptible_50_65 <- susceptible
  } else {
    d$susceptible_65 <- susceptible
  }

  d
}


# take case data and susceptibility data to calculate incidence over time in each vaccine-eligible age group
prep_data <- function(cases, sus) {

  d <- cases %>% merge(sus, by="weeks_since_Jan2020")

  d <- d %>% mutate(age_1 = `[0,12)`/susceptible_under12*100000,
                    age_2 = `[12,18)`/susceptible_12_18*100000,
                    age_3 = `[18,50)`/susceptible_18_50*100000,
                    age_4 = `[50,65)`/susceptible_50_65*100000,
                    age_5 = `[65,Inf)`/susceptible_65*100000)

  d <- d %>% select(!grep("\\[", names(.)))
  d<- d %>% merge(prop_cases, by="weeks_since_Jan2020")

  d
}


# make case predictions from estimated incidence - estimate total immune from natural immunity only
make_case_predictions <- function(data, age, inf_ages, asymp_prop, susceptible, model_type) {
  cases_novacc_update <- NULL
  total_sus <- susceptible

  for (i in 1:nrow(data)) {
    row <- data[i,]
    pred <- row[[age]]/100000

    inf_prop <- as.numeric(row[inf_ages])
    if(sum(inf_prop)!=0){
      inf_prop <- inf_prop/sum(inf_prop)
    }

    cases_no_vacc <- pred*total_sus

    if (model_type=="cases"){
      inf_novacc <- 0

      for (j in 1:length(inf_ages)) {
        inf_novacc <- inf_novacc + cases_no_vacc*inf_prop[j]/(1-asymp_prop[j])
      }

      total_sus <- total_sus - inf_novacc

    } else {
      total_sus <- total_sus - cases_no_vacc
    }

    cases_novacc_update <- c(cases_novacc_update, cases_no_vacc)

  }
  data %>% mutate(cases_novacc = cases_novacc_update)
}


# calculate the difference in cases
difference_cases <- function(data, cases_vacc, cases_novacc) {
  sum(data[[cases_novacc]] - data[[cases_vacc]])
}

