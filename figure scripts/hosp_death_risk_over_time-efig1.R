###################################################################################################
#Title: Plot of hospitalization and death risk over time - eFigure 1
#Authors: Sophia Tan and Hailey Park
###################################################################################################

rm(list=ls())
source("configuration.R")

dates <- read_csv("data/weeks_months_data.csv")
rates <- readRDS("data/hosp_death_rate_week_month_rates.RDS")

rates <- rates %>% filter(months_since_Jan2020 >= 11) %>% group_by(months_since_Jan2020, age_hand_cut) %>%
  summarise(hosp_rate=mean(hosp_rate), death_rate=mean(death_rate))


theme_set(theme(axis.text.x = element_text(angle=90),
                axis.line = element_line(colour = "black"),
                panel.grid.minor = element_blank()))

p1 <- rates %>%
  ggplot(aes(months_since_Jan2020, hosp_rate, group=age_hand_cut, color=age_hand_cut)) +
  geom_line() +
  scale_x_continuous(name = "Month", breaks=0:21, labels=dates$month) +
  scale_color_discrete(labels=c("<12", "12-17", "18-49", "50-64", "65+")) +
  ylab("Monthly hospitalization risk (%)") + ylim(0,30) + labs(color="Age group (years)") +
  theme(legend.title = element_text("Age group (years)"))

p2 <- rates %>%
  ggplot(aes(months_since_Jan2020, death_rate, group=age_hand_cut, color=age_hand_cut)) +
  geom_line() +
  scale_x_continuous(name = "Month", breaks=0:21, labels=dates$month) +
  scale_color_discrete(labels=c("<12", "12-17", "18-49", "50-64", "65+")) +
  ylab("Monthly case fatality risk (%)") + ylim(0,30) + labs(color="Age group (years)") +
  theme(legend.title = element_text("Age group (years)"))


(p1+p2 +  plot_layout(guides = "collect", nrow=1) & theme(legend.position = "bottom")) %>%
  ggsave(filename = "figures/hosp_death_rates_month-efig1.jpg", dpi=300, width=8, height=5)
