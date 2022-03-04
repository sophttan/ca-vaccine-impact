###################################################################################################
#Title: Direct Effects of Vaccination in CA Primary Analysis
#Primary analysis figures
#Author: Sophia Tan
###################################################################################################

rm(list=ls())
source("configuration.R")
source("figure scripts/plot-functions.R")

# model 1 results
res <- readRDS("results/primary/primary-model-main-results.RDS")

dates2 <- read_csv("data/weeks_months_data.csv")

scalar1 <- max(res$ub_12_18)/100
case_1_prim<-plot_averted_cases(res, "[12,18)") +
  geom_line(aes(y=vacc_cum_12_18*scalar1, color="% vaccination")) +
  scale_y_continuous(name="Weekly cases", labels=comma,expand = expansion(mult = c(0, .05)),
                     limits = c(0, max(res$ub_12_18)*1.01),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./scalar1, breaks=seq(0,100,25)))

scalar2 <- max(res$ub_18_50)/100
case_2_prim<- plot_averted_cases(res, "[18,50)") +
  geom_line(aes(y=vacc_cum_18_50*scalar2, color="% vaccination")) +
  scale_y_continuous(name= "Weekly cases",
                     labels=comma,expand = expansion(mult = c(0, .05)),
                     limits = c(0, max(res$ub_18_50)*1.01),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./scalar2, name="Cumulative % vaccination", breaks=seq(0,100,25)))

scalar3 <- max(res$ub_50_65)/100
case_3_prim<- plot_averted_cases(res, "[50,65)") +
  geom_line(aes(y=vacc_cum_50_65*scalar3, color="% vaccination")) +
  scale_y_continuous(name="Weekly cases", labels=comma,expand = expansion(mult = c(0, .05)),
                     limits = c(0, max(res$ub_50_65)*1.01),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./scalar3, breaks=seq(0,100,25)))

scalar4 <- max(res$ub_65)/100
case_4_prim<- plot_averted_cases(res, "[65,Inf)") +
  geom_line(aes(y=vacc_cum_65*scalar4, color="% vaccination")) +
  scale_y_continuous(name= "Weekly cases",
                     labels=comma,expand = expansion(mult = c(0, .05)),
                     limits = c(0, max(res$ub_65)*1.01),
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./scalar4, name="Cumulative % vaccination", breaks=seq(0,100,25)))



vacc_spread <- readRDS("data/vaccination_coverage_data.RDS")
res_12_18 <- readRDS("results/alternative/main/res_12_18_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_18_50 <- readRDS("results/alternative/main/res_18_50_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_50_65 <- readRDS("results/alternative/main/res_50_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)
res_65 <- readRDS("results/alternative/main/res_65_sim.RDS")%>% filter(weeks_since_Jan2020 >= 48)

res_12_18$obs <- res$`[12,18)`
res_18_50$obs <- res$`[18,50)`
res_50_65$obs <- res$`[50,65)`
res_65$obs <- res$`[65,Inf)`

res_12_18$vacc <- vacc_spread$vacc_cum_12_18
res_18_50$vacc <- vacc_spread$vacc_cum_18_50
res_50_65$vacc <- vacc_spread$vacc_cum_50_65
res_65$vacc <- vacc_spread$vacc_cum_65

case_1_alt <- plot_case_predictions(res_12_18) +
  geom_line(aes(y=vacc*scalar1, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    limits = c(0, max(res$ub_12_18)*1.01),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./scalar1, name="Cumulative % vaccination", breaks=seq(0,100,25))
  )

case_2_alt <- plot_case_predictions(res_18_50) +
  geom_line(aes(y=vacc*scalar2, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    limits = c(0, max(res$ub_18_50)*1.01),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./scalar2, name="Cumulative % vaccination", breaks=seq(0,100,25))
  )

case_3_alt <- plot_case_predictions(res_50_65) +
  geom_line(aes(y=vacc*scalar3, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    limits = c(0, max(res$ub_50_65)*1.01),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./scalar3, name="Cumulative % vaccination", breaks=seq(0,100,25))
  )

case_4_alt <- plot_case_predictions(res_65) +
  geom_line(aes(y=vacc*scalar4, color="% vaccination")) +
  geom_vline(xintercept = 74, size=.3, lty="longdash")+
  scale_y_continuous(
    labels=comma, expand = expansion(mult = c(0, .05)),
    limits = c(0, max(res$ub_65)*1.01),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./scalar4, name="Cumulative % vaccination", breaks=seq(0,100,25))
  )


hosp_prim <- readRDS("results/hospitalizations and deaths/hosp_results_primary_model.RDS")
hosp_alt <- readRDS("results/hospitalizations and deaths/hosp_results_alternative_model.RDS")
death_prim <- readRDS("results/hospitalizations and deaths/death_results_primary_model.RDS")
death_alt <- readRDS("results/hospitalizations and deaths/death_results_alternative_model.RDS")


max_hosp <- hosp_prim %>% group_by(age) %>% summarise(max=max(ub_hosp))
max_death <- death_prim %>% group_by(age) %>% summarise(max=max(ub_hosp))

hosp_1_prim <- plot_hosp_death(hosp_prim, "[18,50)", vacc_spread$vacc_cum_18_50, max_hosp$max[1], "Weekly hospitalizations")
hosp_2_prim <- plot_hosp_death(hosp_prim, "[50,65)", vacc_spread$vacc_cum_50_65, max_hosp$max[2], "Weekly hospitalizations")
hosp_3_prim <- plot_hosp_death(hosp_prim, "[65,Inf)", vacc_spread$vacc_cum_65, max_hosp$max[3], "Weekly hospitalizations")

hosp_1_alt <- plot_hosp_death(hosp_alt, "[18,50)", vacc_spread$vacc_cum_18_50, max_hosp$max[1], "Weekly hospitalizations")
hosp_2_alt <- plot_hosp_death(hosp_alt, "[50,65)", vacc_spread$vacc_cum_50_65, max_hosp$max[3], "Weekly hospitalizations")
hosp_3_alt <- plot_hosp_death(hosp_alt, "[65,Inf)", vacc_spread$vacc_cum_65, max_hosp$max[3], "Weekly hospitalizations")

death_1_prim <- plot_hosp_death(death_prim, "[18,50)", vacc_spread$vacc_cum_18_50, max_death$max[1], "Weekly deaths")
death_2_prim <- plot_hosp_death(death_prim, "[50,65)", vacc_spread$vacc_cum_50_65, max_death$max[2], "Weekly deaths")
death_3_prim <- plot_hosp_death(death_prim, "[65,Inf)", vacc_spread$vacc_cum_65, max_death$max[3], "Weekly deaths")

death_1_alt <- plot_hosp_death(death_alt, "[18,50)", vacc_spread$vacc_cum_18_50, max_death$max[1], "Weekly deaths")
death_2_alt <- plot_hosp_death(death_alt, "[50,65)", vacc_spread$vacc_cum_50_65, max_death$max[2], "Weekly deaths")
death_3_alt <- plot_hosp_death(death_alt, "[65,Inf)", vacc_spread$vacc_cum_65, max_death$max[3], "Weekly deaths")


plot_12 <- (case_1_prim+labs(title="12-17 years\nPrimary model", subtitle="A")+
    theme(axis.text.y.right = element_blank()) +
    case_1_alt+labs(title="\nAlternative model", subtitle="B")+
    theme(axis.title.y.left = element_blank(), axis.text.y.left = element_blank()))+plot_layout(guides="collect") &theme(legend.position = "bottom")
plot_12 %>%
  ggsave(filename="figures/12-17-results-efig3.jpg", dpi=300, height=5, width=7)



plot_18 <- (case_2_prim+labs(title="18-49 years\nPrimary model", subtitle="A")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                                      axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              case_2_alt+labs(title="\nAlternative model", subtitle="B")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              hosp_1_prim+labs(subtitle="C")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                   axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              hosp_1_alt+labs(subtitle="D")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              death_1_prim+labs(subtitle="E")+theme(axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              death_1_alt+labs(subtitle="F"))+theme(axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
  plot_layout(ncol=2, nrow=3)

(plot_18 + plot_layout(guides="collect")&theme(legend.position = "bottom")) %>%
  ggsave(filename="figures/18-49-results-efig4.jpg", dpi=300, height=10, width=8.5)



plot_50 <- (case_3_prim+labs(title="50-64 years\nPrimary model", subtitle="A")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                                      axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              case_3_alt+labs(title="\nAlternative model",subtitle="B")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              hosp_2_prim+labs(subtitle="C")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                   axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              hosp_2_alt+labs(subtitle="D")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              death_2_prim+labs(subtitle="E")+theme(axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              death_2_alt+labs(subtitle="F"))+theme(axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
  plot_layout(ncol=2, nrow=3)
(plot_50 + plot_layout(guides="collect")&theme(legend.position = "bottom"))%>%
  ggsave(filename="figures/50-64-results-efig5.jpg", dpi=300, height=10, width=8.5)



plot_65 <- (case_4_prim+labs(title="65+ years\nPrimary model", subtitle="A")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                                      axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              case_4_alt+labs(title="\nAlternative model",subtitle="B")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              hosp_3_prim+labs(subtitle="C")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                   axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              hosp_3_alt+labs(subtitle="D")+theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                  axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
              death_3_prim+labs(subtitle="E")+theme(axis.title.y.right = element_blank(), axis.text.y.right=element_blank())+
              death_3_alt+labs(subtitle="F"))+theme(axis.title.y.left = element_blank(), axis.text.y.left=element_blank())+
  plot_layout(ncol=2, nrow=3)
(plot_65 + plot_layout(guides="collect")&theme(legend.position = "bottom"))%>%
  ggsave(filename="figures/65-results-efig6.jpg", dpi=300, height=10, width=8.5)
