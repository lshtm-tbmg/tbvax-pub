# ------------------------
# ICER comparison for the Vaccine Characteristic and Coverage Scenarios - Figure 3
# Rebecca Clark
# 7 June 2024
# ------------------------

rm(list=ls(all=TRUE))

suppressPackageStartupMessages({
  options(scipen=999)
  library(here)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  library(data.table)
  library(scales)
  library(dplyr)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
  i <- "DEL"
  
  ochalek_lb <- 328
  ochalek_ub <- 443
  gdp_capita <- 1928
  
})

# Data for plots
if (T){
  uid_counter_2=0
  incremental_all <- c()
  setwd(here("Econ/TBVAX econ output"))
  
  ## Load model results
  econ_output <- fread(paste0("./", i,"_hs_cea.csv"), header=TRUE, check.names=FALSE)
  UID_list <- unique(econ_output$UID)
  
  # Sum costs over the years 
  scenario_level_all <- aggregate(cbind(dalys_d, cost_d) ~ UID + scenario, data = econ_output,
                                  FUN = sum, na.rm = TRUE)
  
  for (j0 in 1:1000) {
    j = UID_list[j0]
    print(j)
    
    scenario_level  <- subset(scenario_level_all, UID==j)
    incremental     <- as.data.frame(unique(scenario_level$scenario))
    incremental$UID <- j
    
    # remove baseline row, subtract intervention from baseline to get incremental costs and DALYs
    incremental = incremental[-1,]
    incremental_output <- scenario_level[-c(1:2)]
    incremental_output <- incremental_output[-1 , ] - incremental_output[rep(1, nrow(incremental_output) - 1), ]
    incremental <- cbind(incremental, incremental_output)
    
    # Calculate ICERs, DALYs averted
    incremental <-  mutate(incremental, dalys_d_averted = dalys_d*-1)
    
    incremental_all = rbind(incremental_all, incremental)
    
    uid_counter_2 <- uid_counter_2 + 1
    print(uid_counter_2) 
  }
  
  
  #Renaming first column name 
  name <- paste0("unique(scenario_level$scenario)")
  names(incremental_all)[names(incremental_all) == name] <- "scenario"
  
  setwd(here("Econ/TBVAX econ output"))
  write.csv(incremental_all, paste0("incremental_sum_",i,".csv"))
  
  setwd(here("Econ"))
  
  #For each scenario, mean and 2.5/97.5 percentile of all columns across UIDs 
  q = c(.025, .5, .975)
  
  #calculate quantiles by grouping variable
  econ_data <-
    incremental_all %>%
    group_by(scenario) %>%
    summarize(
      inc_dalys_5 = quantile(dalys_d, probs = q[1]), 
      inc_dalys_95 = quantile(dalys_d, probs = q[3]),
      inc_dalys_mean = mean(dalys_d),
      
      inc_cost_d_5 = quantile(cost_d, probs = q[1]), 
      inc_cost_d_95 = quantile(cost_d, probs = q[3]),
      inc_cost_d_mean = mean(cost_d),
    )
  
  fwrite(econ_data, paste0("TBVAX econ output/econ_data_sum_",i,".csv"))
  
}

setwd(here("Econ"))

GUJ_econ_alldata <- fread(here("Econ/TBVAX econ output/econ_data_sum_GUJ.csv"))
GUJ_econ_alldata <- GUJ_econ_alldata[, Region := "Gujarat"]

DEL_econ_alldata <- fread(here("Econ/TBVAX econ output/econ_data_sum_DEL.csv"))
DEL_econ_alldata <- DEL_econ_alldata[, Region := "Delhi"]

econ_alldata <- rbind(DEL_econ_alldata, GUJ_econ_alldata)
econ_alldata <- econ_alldata[grepl("BCG", scenario), Vaccine := "BCG-revaccination"]
econ_alldata <- econ_alldata[grepl("M72", scenario), Vaccine := "M72/AS01E"]

# Label scenarios
if (T){
  econ_alldata$scenario <- 
    ordered(econ_alldata$scenario,
            levels = c("baseline", "M72_AI_POD_50_10yr_med_2030",
                       "M72_AI_POD_60_10yr_med_2030", "M72_AI_POD_70_10yr_med_2030",
                       "M72_AI_POD_50_5yr_med_2030", "M72_AI_POD_50_15yr_med_2030",
                       "M72_AI_POD_50_20yr_med_2030", "M72_AI_POID_50_10yr_med_2030",
                       "M72_CI_POD_50_10yr_med_2030", "M72_AI_POD_50_10yr_med_2036", 
                       "M72_AI_POD_50_10yr_low_2030", "M72_AI_POD_50_10yr_high_2030",
                       "M72_AI_POD_50_10yr_med_2030_diffages", "M72_AI_POD_50_10yr_med_2030_alladults",
                       "BCG_NCI_POI_45_10yr_med_2025", "BCG_NCI_POI_70_10yr_med_2025",
                       "BCG_NCI_POI_45_5yr_med_2025", "BCG_NCI_POI_45_15yr_med_2025", 
                       "BCG_NCI_POI_45_20yr_med_2025", "BCG_NCI_POID_45_10yr_med_2025",
                       "BCG_AI_POI_45_10yr_med_2025", "BCG_NCI_POI_45_10yr_med_2031",
                       "BCG_NCI_POI_45_10yr_low_2025", "BCG_NCI_POI_45_10yr_high_2025",
                       "BCG_NCI_POI_45_10yr_med_2025_diffages", "BCG_NCI_POI_45_10yr_med_2025_alladults"),
            labels = c("No-New-Vaccine","Basecase", "60% efficacy", "70% efficacy", 
                       "5 years protection", "15 years protection", "20 years protection",
                       "Prevention of infection and disease", "Efficacy with current infection at vaccination",
                       "2036 introduction", "Lower coverage", "Higher coverage", 
                       "Older ages (campaign for ages 18-55)", "All adults (campaign for ages 19+, routine age 18)",
                       "Basecase", "70% efficacy",  "5 years protection",
                       "15 years protection", "20 years protection", "Prevention of infection and disease",
                       "Efficacy with any infection at vaccination", "2031 introduction",
                       "Lower coverage", "Higher coverage",
                       "Older ages (campaign for ages 16-34, routine age 15)",
                       "All adults (campaign for ages 19+, routine age 18)"))
  
  econ_alldata$Vaccine <- 
    ordered(econ_alldata$Vaccine,
            levels = c("M72/AS01E","BCG-revaccination"),
            labels = c("M72/AS01[E]", "BCG-revaccination"))
  
  include <- c("Basecase", "Prevention of infection and disease",
               "Efficacy with any infection at vaccination",
               "Efficacy with current infection at vaccination")
  
  
}


## Figure 3 Comparison of ICERs for select Vaccine Characteristic 
##          and Coverage Scenarios.

ggplot(econ_alldata[scenario %in% include]) +
  geom_point(aes(x = (inc_dalys_mean*-1)*(1000/1e6), y = (inc_cost_d_mean)*(1000/1e6), col = Vaccine), size = 3) +
  geom_abline(intercept = 0, slope = gdp_capita, colour = "black") +
  geom_abline(intercept = 0, slope = ochalek_ub, colour = "black", lty = 5) +
  geom_abline(intercept = 0, slope = ochalek_lb, colour = "black", lty = 3) +
  facet_grid(Region ~ Vaccine, scales = "free_y", labeller = label_parsed) +
  theme_minimal_grid() + panel_border(color = "black") + 
  labs(x = "Incremental DALYs averted (millions)") +
  labs (y = "Incremental costs (USD$, millions)") +
  guides(col = guide_legend(ncol=1)) +
  scale_colour_manual(values = c("M72/AS01[E]" = "#541352FF", "BCG-revaccination" = "#2f9aa0FF")) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(hjust = 1, size = 14),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 12),
    strip.text.x = element_text(size = 18),
    strip.text.y = element_text(size = 18)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.1), label = comma) +
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 500), label = comma)


# ---- end
