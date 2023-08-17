#-------------------
# Post process impact
# Rebecca Clark
# Updated 17 August 2023
#-------------------

# 1. Calculate the cumulative number of cases/treatments/deaths over the
#       years for each scenario
# 2. Calculate the cumulative number of cases/treatments/deaths AVERTED over
#       the years for each intervention scenarios compared to the baseline scenario


# 0. Set-up
rm(list=ls())

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(renv)
  library(arrow)
  library(magrittr)
})

# Create a folder to save the output
if (!dir.exists(here("epi_output/grouped_output"))) dir.create(here("epi_output/grouped_output"))

# Load in data generated from Generate_Impact_Example.R
n_epi <- open_dataset(sources = "./epi_output/n_epi/") %>% dplyr::collect()


##### 1. Calculate the cumulative number of cases/treatments/deaths

sum_plots <- n_epi[, `:=`(sum_tx = cumsum(N_tx),
                          sum_inc = cumsum(N_inc),
                          sum_mort = cumsum(N_mort)),
                   by = .(AgeGrp, Runtype, UID)]

sum_plots <- sum_plots[, .(UID, Year, AgeGrp, Runtype,
                           N_inc, N_mort, N_tx,
                           sum_inc, sum_mort, sum_tx)]

# Melt and then calculate the median, low and high bounds
sum_plots_long <- melt(sum_plots, id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                       measure.vars = c("sum_inc", "sum_mort", "sum_tx"),
                       variable.name = "Indicator", value.name = "Value")

sum_plots_long <- sum_plots_long[, .(medval = median(Value),
                                     lowval = quantile(Value, 0.025),
                                     highval = quantile(Value, 0.975)),
                                 .(Year, AgeGrp, Runtype, Indicator)]

fwrite(sum_plots_long, "epi_output/grouped_output/cumulative_numbers_output.csv")


###### 2. Cumulative number of cases/treatments/deaths AVERTED between scenario and baseline 

sum_epi_long <- melt(sum_plots, measure.vars = c("sum_tx", "sum_inc", "sum_mort"), 
                     id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                     value.name = "Value", variable.name = "Indicator")

sum_epi_wide <- dcast(sum_epi_long, Year + UID + AgeGrp + Indicator ~ Runtype,
                      value.var = "Value")

vx_scenarios <- unique(sum_epi_long$Runtype)
vx_scenarios <- vx_scenarios[vx_scenarios  != "baseline"]

for (scen in vx_scenarios) {
  set(x = sum_epi_wide, j = paste0("Diff_", scen), value = sum_epi_wide[["baseline"]] - sum_epi_wide[[paste0(scen)]])
}


sum_averted <- melt(data = sum_epi_wide, measure.vars = patterns("^Diff.*"), 
                    id.vars = c("Year", "UID", "AgeGrp", "Indicator"),
                    value.name = "Value", variable.name = "Runtype")

sum_averted <- sum_averted[Indicator == "sum_inc",  Indicator := "inc_avert"]
sum_averted <- sum_averted[Indicator == "sum_tx",   Indicator := "tx_avert"]
sum_averted <- sum_averted[Indicator == "sum_mort", Indicator := "mort_avert"]

sum_averted <- sum_averted[, .(medval = median(Value),
                               lowval = quantile(Value, 0.025),
                               highval = quantile(Value, 0.975)),
                           by = .(Year, Runtype, Indicator, AgeGrp)]


sum_averted <- sum_averted[, combined := paste0(round(medval, 1),
                                                " (", round(lowval, 1),
                                                ", ", round(highval, 1), ")")]

sum_averted$Runtype <- gsub("Diff_", "", as.character(sum_averted$Runtype))

fwrite(sum_averted, "epi_output/grouped_output/cumulative_numbers_averted_output.csv")



# ---- end


