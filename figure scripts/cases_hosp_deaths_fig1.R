###################################################################################################
#Title: Plot of COVID-19 cases, hospitalizations, and deaths over time (Figure 1)
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")

dates <- read_csv("data/weeks_months_data.csv") %>% select(!X1)
vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")

theme_set(theme(legend.title=element_blank(), axis.text.x = element_text(angle=90),
                axis.line = element_line(colour = "black"),
                panel.grid.minor = element_blank()))

hosp_counts <- readRDS("data/hosp_death_rate_week_month_rates.RDS") %>% select(!age_hand_cut) %>%  group_by(weeks_since_Jan2020) %>% summarise_all(sum)
hosp_counts <- hosp_counts %>% left_join(vacc_spread, "weeks_since_Jan2020") %>% replace_na(list(vacc_cum=0))

scalar1 <- 320000/100#max(hosp_counts$cases)/100
cases <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, cases)) + geom_line(aes(color="Outcome")) +
  geom_line(aes(y=vacc_cum*scalar1, color="Cumulative vaccination (%)")) +
  geom_vline(xintercept = 74.8, size=.3, lty="longdash") +
  scale_x_continuous(name = "Month", breaks=seq(0,94,4.4), labels=dates$month) +
  scale_y_continuous(labels=comma, name = "Weekly outcome", expand = expansion(mult=c(0,0)), limits = c(0,320000),
                     breaks=seq(0,320000,320000/4),
                     sec.axis = sec_axis(~./scalar1, breaks=seq(0,100,25))) +
  labs(title="A", subtitle="Cases")

scalar2 <- 12500/100#max(hosp_counts$hosp)/100
hosp <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, hosp)) + geom_line(aes(color="Outcome")) +
  scale_x_continuous(name = "Month", breaks=seq(0,94,4.4), labels=dates$month) +
  geom_line(aes(x=weeks_since_Jan2020, y=vacc_cum*scalar2, color="Cumulative vaccination (%)")) +
  geom_vline(xintercept = 74.8, size=.3, lty="longdash") +
  scale_y_continuous(labels=comma, name=NULL, expand = expansion(mult=c(0,0)), limits = c(0,12500),
                     sec.axis = sec_axis(~./scalar2, breaks=seq(0,100,25))) +
  labs(title="B", subtitle="Hospitalizations")

scalar3 <- 5000/100#max(hosp_counts$died)/100
died <- hosp_counts %>% ggplot(aes(weeks_since_Jan2020, died)) + geom_line(aes(color="Outcome")) +
  scale_x_continuous(name = "Month", breaks=seq(0,94,4.4), labels=dates$month) +
  geom_vline(xintercept = 74.8, size=.3, lty="longdash") +
  geom_line(aes(x=weeks_since_Jan2020, y=vacc_cum*scalar3, color="Cumulative vaccination (%)")) +
  scale_y_continuous(labels=comma, name=NULL, expand = expansion(mult=c(0,0)), limits = c(0,5000),
                     sec.axis = sec_axis(~./scalar3, breaks=seq(0,100,25), name="Cumulative vaccine coverage (%)")) +
  labs(title = "C", subtitle="Deaths")

(cases+hosp+died+
  plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>%
  ggsave(filename="figures/obs_cases_hosp_deaths-fig1.jpg", dpi=300, width = 10, height=4)

library(devEMF)
emf(file="figures/obs_cases_hosp_deaths-fig1.emf", width = 10, height=4)
(cases+hosp+died+
    plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom"))
dev.off()
