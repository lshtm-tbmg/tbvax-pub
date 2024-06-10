# ------------------------
# Competing choice CEA for Delhi, Gujarat - Figure 2
# Rebecca Clark
# 7 June 2024
# ------------------------

rm(list=ls(all=TRUE))

suppressPackageStartupMessages({
  options(scipen=999)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  library(data.table)
  library(scales)
  library(dplyr)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
})

# Cost-effectiveness analysis for Policy Scenarios
if (T){
  
  setwd(here("Econ/TBVAX econ output"))
  
  ## Load model results
  i <- "DEL" #### change between "DEL" and "GUJ"
  econ_output <- fread(paste0("./", i, "_hs_cea.csv"), header=TRUE, check.names=FALSE)
  
  # Sum costs and dalys over the years 
  scenario_all <- econ_output[, .(dalys_d = sum(dalys_d), cost_d = sum(cost_d)),
                              by = .(UID, scenario)]
  
  # Average the costs and dalys
  scenario_all <- scenario_all[, .(cost_d_mean = mean(cost_d), dalys_d_mean = mean(dalys_d)),
                               by = .(scenario)]
  
  scenario_M72 <- scenario_all[grepl("M72", scenario) | scenario == "baseline"]
  scenario_BCG <- scenario_all[grepl("BCG", scenario) | scenario == "baseline"]
  
  if (T){
    scenario_M72$scenario <- 
      ordered(scenario_M72$scenario,
              levels = c("baseline", "M72_AI_POD_50_10yr_med_2030",
                         "M72_AI_POD_60_10yr_med_2030", "M72_AI_POD_70_10yr_med_2030",
                         "M72_AI_POD_50_5yr_med_2030", "M72_AI_POD_50_15yr_med_2030",
                         "M72_AI_POD_50_20yr_med_2030", "M72_AI_POID_50_10yr_med_2030",
                         "M72_CI_POD_50_10yr_med_2030", "M72_AI_POD_50_10yr_med_2036", 
                         "M72_AI_POD_50_10yr_low_2030", "M72_AI_POD_50_10yr_high_2030",
                         "M72_AI_POD_50_10yr_med_2030_diffages", "M72_AI_POD_50_10yr_med_2030_eld",
                         "M72_AI_POD_50_10yr_med_2030_alladults"),
              labels = c("No-New-Vaccine", "Basecase", "60% efficacy", "70% efficacy", 
                         "5 years duration of protection", "15 years duration of protection",
                         "20 years duration of protection", "Prevention of infection and disease",
                         "Efficacy with current infection at vaccination", "2036 introduction",
                         "Lower coverage", "Higher coverage", "Routine age 17, campaign for ages 18-55",
                         "Routine age 60, campaign for ages 61+", "Routine age 18, campaign for ages 19+"))
    
    scenario_BCG$scenario <- 
      ordered(scenario_BCG$scenario,
              levels = c("baseline", "BCG_NCI_POI_45_10yr_med_2025", "BCG_NCI_POI_70_10yr_med_2025",
                         "BCG_NCI_POI_45_5yr_med_2025", "BCG_NCI_POI_45_15yr_med_2025",
                         "BCG_NCI_POI_45_20yr_med_2025", "BCG_NCI_POID_45_10yr_med_2025",
                         "BCG_AI_POI_45_10yr_med_2025", "BCG_NCI_POI_45_10yr_med_2031",
                         "BCG_NCI_POI_45_10yr_low_2025", "BCG_NCI_POI_45_10yr_high_2025",
                         "BCG_NCI_POI_45_10yr_med_2025_diffages", "BCG_NCI_POI_45_10yr_med_2025_eld",
                         "BCG_NCI_POI_45_10yr_med_2025_alladults"),
              labels = c("No-New-Vaccine", "Basecase", "70% efficacy", "5 years duration of protection",
                         "15 years duration of protection", "20 years duration of protection",
                         "Prevention of infection and disease", "Efficacy with any infection at vaccination",
                         "2031 introduction", "Lower coverage", "Higher coverage",
                         "Routine age 15, campaign for ages 16-34", "Routine age 60, campaign for ages 61+",
                         "Routine age 18, campaign for ages 19+"))
  }
  
  if(i == "GUJ"){
    GUJ_scenario_M72 <- scenario_M72
    GUJ_scenario_BCG <- scenario_BCG
  } else if (i == "DEL"){
    DEL_scenario_M72 <- scenario_M72
    DEL_scenario_BCG <- scenario_BCG
  }
}  

# Gujarat: Set up efficiency plane with Policy Scenarios
if (T){
  M72_competing_GUJ <- GUJ_scenario_M72[grepl("ages", scenario) | scenario == "No-New-Vaccine" | scenario == "Basecase"]
  M72_competing_GUJ <- M72_competing_GUJ[, total_dalys_averted := M72_competing_GUJ[scenario == "No-New-Vaccine"]$dalys_d_mean - dalys_d_mean]
  
  seg1_start_x_GUJ <- M72_competing_GUJ[scenario == "No-New-Vaccine"]$cost_d_mean
  seg1_start_y_GUJ <- M72_competing_GUJ[scenario == "No-New-Vaccine"]$total_dalys_averted
  
  seg1_end_x_GUJ <- M72_competing_GUJ[scenario == "Routine age 18, campaign for ages 19+"]$cost_d_mean
  seg1_end_y_GUJ <- M72_competing_GUJ[scenario == "Routine age 18, campaign for ages 19+"]$total_dalys_averted
  
  M72_competing_GUJ <- M72_competing_GUJ[scenario == "Basecase", scenario := "Routine age 15, campaign for ages 16-34"]
  
  BCG_competing_GUJ <- scenario_BCG[grepl("ages", scenario) | scenario == "No-New-Vaccine" | scenario == "Basecase"]
  
  BCG_competing_GUJ <- BCG_competing_GUJ[, total_dalys_averted := 
                                           BCG_competing_GUJ[scenario == "No-New-Vaccine"]$dalys_d_mean - dalys_d_mean]
  
  seg1_start_x_BCG_GUJ <- BCG_competing_GUJ[scenario == "No-New-Vaccine"]$cost_d_mean
  seg1_start_y_BCG_GUJ <- BCG_competing_GUJ[scenario == "No-New-Vaccine"]$total_dalys_averted
  
  seg1_end_x_BCG_GUJ <- BCG_competing_GUJ[scenario == "Basecase"]$cost_d_mean
  seg1_end_y_BCG_GUJ <- BCG_competing_GUJ[scenario == "Basecase"]$total_dalys_averted
  
  seg2_end_x_BCG_GUJ <- BCG_competing_GUJ[scenario == "Routine age 15, campaign for ages 16-34"]$cost_d_mean
  seg2_end_y_BCG_GUJ <- BCG_competing_GUJ[scenario == "Routine age 15, campaign for ages 16-34"]$total_dalys_averted
  
  seg3_end_x_BCG_GUJ <- BCG_competing_GUJ[scenario == "Routine age 18, campaign for ages 19+"]$cost_d_mean
  seg3_end_y_BCG_GUJ <- BCG_competing_GUJ[scenario == "Routine age 18, campaign for ages 19+"]$total_dalys_averted
  
  BCG_competing_GUJ <- BCG_competing_GUJ[scenario == "Basecase", scenario := "Routine age 10, campaign for ages 11-18"]
  
}

# Delhi: Set up efficiency plane with Policy Scenarios
if (T){
  M72_competing_DEL <- DEL_scenario_M72[grepl("ages", scenario) | scenario == "No-New-Vaccine" | scenario == "Basecase"]
  M72_competing_DEL <- M72_competing_DEL[, total_dalys_averted := M72_competing_DEL[scenario == "No-New-Vaccine"]$dalys_d_mean - dalys_d_mean]
  
  seg1_start_x_DEL <- M72_competing_DEL[scenario == "No-New-Vaccine"]$cost_d_mean
  seg1_start_y_DEL <- M72_competing_DEL[scenario == "No-New-Vaccine"]$total_dalys_averted
  
  seg1_end_x_DEL <- M72_competing_DEL[scenario == "Basecase"]$cost_d_mean
  seg1_end_y_DEL <- M72_competing_DEL[scenario == "Basecase"]$total_dalys_averted
  
  seg2_end_x_DEL <- M72_competing_DEL[scenario == "Routine age 17, campaign for ages 18-55"]$cost_d_mean
  seg2_end_y_DEL <- M72_competing_DEL[scenario == "Routine age 17, campaign for ages 18-55"]$total_dalys_averted
  
  seg3_end_x_DEL <- M72_competing_DEL[scenario == "Routine age 18, campaign for ages 19+"]$cost_d_mean
  seg3_end_y_DEL <- M72_competing_DEL[scenario == "Routine age 18, campaign for ages 19+"]$total_dalys_averted
  
  M72_competing_DEL <- M72_competing_DEL[scenario == "Basecase", scenario := "Routine age 15, campaign for ages 16-34"]
  
  BCG_competing_DEL <- scenario_BCG[grepl("ages", scenario) | scenario == "No-New-Vaccine" | scenario == "Basecase"]
  
  BCG_competing_DEL <- BCG_competing_DEL[, total_dalys_averted := 
                                           BCG_competing_DEL[scenario == "No-New-Vaccine"]$dalys_d_mean - dalys_d_mean]
  
  seg1_start_x_BCG_DEL <- BCG_competing_DEL[scenario == "No-New-Vaccine"]$cost_d_mean
  seg1_start_y_BCG_DEL <- BCG_competing_DEL[scenario == "No-New-Vaccine"]$total_dalys_averted
  
  seg1_end_x_BCG_DEL <- BCG_competing_DEL[scenario == "Basecase"]$cost_d_mean
  seg1_end_y_BCG_DEL <- BCG_competing_DEL[scenario == "Basecase"]$total_dalys_averted
  
  BCG_competing_DEL <- BCG_competing_DEL[scenario == "Basecase", scenario := "Routine age 10, campaign for ages 11-18"]
}


## Figure 2 Competing choice cost-effectiveness analysis for 
##          Delhi and Gujarat Policy Scenarios for both vaccine products.

plt <- list()

plt[[1]] <- ggplot(M72_competing_GUJ) +
  theme_minimal_grid() + panel_border(color = "black") + 
  geom_segment(aes(y = seg1_start_x_GUJ*1000/1e9, x = seg1_start_y_GUJ*1000/1e6,
                   yend = seg1_end_x_GUJ*1000/1e9, xend = seg1_end_y_GUJ*1000/1e6), lty = 2, col = "gray50") +
  geom_point(aes(y = cost_d_mean*1000/1e9, x = total_dalys_averted*1000/1e6, col = scenario), size = 4) +
  geom_text(aes(y = 0.9, x = 0.6, label = "ICER = $975"), size = 4, col = "black") +
  labs(x = "Total DALYs averted (millions)", y = "Total costs (US$ billions)",
       title = "Gujarat: M72/AS01E") +
  scale_y_continuous(label = comma, limits = c(0, 1.6)) +
  scale_x_continuous(label = comma, limits = c(NA, 2)) +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = c(0.9, 0.2)
  )

plt[[2]] <- ggplot(BCG_competing_GUJ) +
  theme_minimal_grid() + panel_border(color = "black") + 
  geom_segment(aes(y = seg1_start_x_BCG_GUJ*1000/1e9, x = seg1_start_y_BCG_GUJ*1000/1e6,
                   yend = seg1_end_x_BCG_GUJ*1000/1e9, xend = seg1_end_y_BCG_GUJ*1000/1e6), lty = 2, col = "gray50") +
  geom_segment(aes(y = seg1_end_x_BCG_GUJ*1000/1e9, x = seg1_end_y_BCG_GUJ*1000/1e6, 
                   yend = seg2_end_x_BCG_GUJ*1000/1e9, xend = seg2_end_y_BCG_GUJ*1000/1e6), lty = 2, col = "gray50") +
  geom_segment(aes(y = seg2_end_x_BCG_GUJ*1000/1e9, x = seg2_end_y_BCG_GUJ*1000/1e6, 
                   yend = seg3_end_x_BCG_GUJ*1000/1e9, xend = seg3_end_y_BCG_GUJ*1000/1e6), lty = 2, col = "gray50") +
  geom_point(aes(y = cost_d_mean*1000/1e9, x = total_dalys_averted*1000/1e6, col = scenario), size = 4) +
  geom_text(aes(x = 0.3, y = 0.55, label = "ICER = $351"), size = 4, col = "black") +
  geom_text(aes(x = 0.5, y = 0.65, label = "ICER = $868"), size = 4, col = "black") +
  geom_text(aes(x = 0.6, y = 0.8, label = "ICER = $3,486"), size = 4, col = "black") +
  labs(x = "Total DALYs averted (millions)", y = "Total costs (US$ billions)", 
       title = "Gujarat: BCG-revaccination") +
  scale_y_continuous(label = comma, limits = c(0, 1.6)) +
  scale_x_continuous(label = comma, limits = c(0, 2)) +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = c(0.9, 0.2)
  )


plt[[3]] <- ggplot(M72_competing_DEL) +
  theme_minimal_grid() + panel_border(color = "black") + 
  geom_segment(aes(y = seg1_start_x_DEL*1000/1e9, x = seg1_start_y_DEL*1000/1e6,
                   yend = seg1_end_x_DEL*1000/1e9, xend = seg1_end_y_DEL*1000/1e6), lty = 2, col = "gray50") +
  geom_segment(aes(y = seg1_end_x_DEL*1000/1e9, x = seg1_end_y_DEL*1000/1e6, 
                   yend = seg2_end_x_DEL*1000/1e9, xend = seg2_end_y_DEL*1000/1e6), lty = 2, col = "gray50") +
  geom_segment(aes(y = seg2_end_x_DEL*1000/1e9, x = seg2_end_y_DEL*1000/1e6, 
                   yend = seg3_end_x_DEL*1000/1e9, xend = seg3_end_y_DEL*1000/1e6), lty = 2, col = "gray50") +
  geom_point(aes(y = cost_d_mean*1000/1e9, x = total_dalys_averted*1000/1e6, col = scenario), size = 4) +
  labs(x = "Total DALYs averted (millions)", y = "Total costs (US$ billions)",
       title = "Delhi: M72/AS01E") +
  scale_y_continuous(label = comma, limits = c(0, 1.6)) +
  scale_x_continuous(label = comma, limits = c(NA, 2)) +
  geom_text(aes(x = 0.4, y = 0.9, label = "ICER = US$4"), size = 4, col = "black") +
  geom_text(aes(x = 1.8, y = 0.9, label = "ICER = US$126"), size = 4, col = "black") +
  geom_text(aes(x = 1.5, y = 1.2, label = "ICER = US$317"), size = 4, col = "black") +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = c(0.9, 0.2)
  )

plt[[4]] <- ggplot(BCG_competing_DEL) +
  theme_minimal_grid() + panel_border(color = "black") + 
  geom_segment(aes(y = seg1_start_x_BCG_DEL*1000/1e9, x = seg1_start_y_BCG_DEL*1000/1e6,
                   yend = seg1_end_x_BCG_DEL*1000/1e9, xend = seg1_end_y_BCG_DEL*1000/1e6), lty = 2, col = "gray50") +
  geom_point(aes(x = total_dalys_averted*1000/1e6, y = cost_d_mean*1000/1e9, col = scenario), size = 4) +
  labs(x = "Total DALYs averted (millions)", y = "Total costs (US$ billions)", 
       title = "Delhi: BCG-revaccination") +
  scale_y_continuous(label = comma, limits = c(0, 1.6)) +
  scale_x_continuous(label = comma, limits = c(NA, 2)) +
  geom_text(aes(x = 0.4, y = 0.85, label = "ICER = cost-saving"), size = 4, col = "black") +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = c(0.9, 0.2)
  )


patchwork::wrap_plots(plt, ncol = 2, nrow = 2)


# ---- end

