#-------------------
# Generate Epi Uncertainty
# Rebecca Clark
# Last updated 23 January 2023
#-------------------

# 1. Data for plotting the incidence and mortality rates over time
# 2. Calculate IRR and MRR for each year
# 3. Plot cumulative tx, cases, deaths averted over time
# 4. Calculate cumulative tx, cases, deaths averted by each year


# 1. Set-up
suppressPackageStartupMessages({
  rm(list=ls())
  library(here)
  library(data.table)
  library(renv)
  library(arrow)
  library(magrittr)
})

# Create a folder to save the output
if (!dir.exists(here("epi_output/grouped_output"))) dir.create(here("epi_output/grouped_output"))

# Load in data generated from GenEpiOutput_complex.R
n_epi <- open_dataset(sources = "./epi_output/n_epi/") %>% dplyr::collect()


#-------------------------------------------------------------------------------------------

# 1. Data for plotting incidence and mortality rates over time, and 
#     number of cases, treatments, and deaths over time

# (subset to 2023 here because that's one year before vx introduced, and all 
#   scenarios have output for that year)

rate_plots <- n_epi[Year >= 2023] 
rate_plots <- rate_plots[, `:=`(inc_rate = (N_inc/Population)*(100000),
                                mort_rate = (N_mort/Population)*(100000)),
                         by = .(Year, AgeGrp, Runtype, UID)]

sum_plots<- n_epi[Year >= 2023] 
sum_plots <- sum_plots[, `:=`(sum_tx = cumsum(N_tx),
                              sum_inc = cumsum(N_inc),
                              sum_mort = cumsum(N_mort)),
                       by = .(AgeGrp, Runtype, UID)]

raw_plots <- merge(sum_plots, rate_plots)

raw_plots <- raw_plots[, .(UID, Year, AgeGrp, Runtype, Population,
                           N_inc, N_mort, N_tx, inc_rate, mort_rate, sum_inc, sum_mort, sum_tx)]

# Melt and then calculate the median, low and high bounds for Incidence and Mortality rates
raw_plots_long <- melt(raw_plots, id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                       measure.vars = c("inc_rate", "mort_rate", "sum_inc", "sum_mort", "sum_tx"),
                       variable.name = "Indicator", value.name = "Value")

raw_plots_long <- raw_plots_long[, .(medval = median(Value),
                                     lowval = quantile(Value, 0.025),
                                     highval = quantile(Value, 0.975)),
                                 .(Year, AgeGrp, Runtype, Indicator)]

fwrite(raw_plots_long, "epi_output/grouped_output/raw_output.csv")


#-------------------------------------------------------------------------------------------

# 2. Data for calculating incidence and mortality rate reductions (compared to baseline)

# Melt to long form (create another variable that is either inc_rate or mort_rate)
rate_red_long <- melt(rate_plots, measure.vars = c("inc_rate", "mort_rate"), 
                      id.vars = c("Year", "AgeGrp", "Runtype", "UID"),
                      value.name = "Value", variable.name = "Indicator")


# Cast to wide form to calculate the rate reductions
rate_red_wide <- dcast(rate_red_long, Year + UID + AgeGrp + Indicator ~ Runtype,
                       value.var = "Value")


# Calculate the rate reductions (scenario compared to baseline)
vx_scenarios <- unique(rate_plots$Runtype)
vx_scenarios <- vx_scenarios[vx_scenarios != "baseline"]

for (scen in vx_scenarios) {
  set(x = rate_red_wide, j = paste0(scen, "_diff"), value = rate_red_wide[["baseline"]] - rate_red_wide[[paste0(scen)]])
  set(x = rate_red_wide, j = paste0("PER_", scen), value = rate_red_wide[[paste0(scen, "_diff")]] / rate_red_wide[["baseline"]])
}


# Melt back to long form with the rate reduction variable
rate_reductions <- melt(data = rate_red_wide, measure.vars = patterns("^PER.*"), 
                        id.vars = c("UID", "AgeGrp", "Year", "Indicator"),
                        value.name = "Value", variable.name = "Runtype")

rate_reductions <- rate_reductions[!(is.na(Value))]

rate_reductions <- rate_reductions[Indicator == "inc_rate", Indicator := "inc_RR"]
rate_reductions <- rate_reductions[Indicator == "mort_rate", Indicator := "mort_RR"]

# Calculate the median, upper, and lower bounds (Uncertainty from the UIDs)
rate_reductions <- rate_reductions[, .(medval = median(Value),
                                       lowval = quantile(Value, 0.025),
                                       highval = quantile(Value, 0.975)),
                                   by = .(Year, Runtype, AgeGrp, Indicator)]


rate_reductions <- rate_reductions[, combined := paste0(round(medval*100, 1),
                                                        "% (", round(lowval*100, 1),
                                                        ", ", round(highval*100, 1), ")")]

rate_reductions$Runtype <- gsub("PER_", "", as.character(rate_reductions$Runtype))



#-------------------------------------------------------------------------------------------

# 3.  Data for calculating cumulative tx, cases, and deaths averted by specific years

# Cumulative number of tx/inc/mort between vaccine introduction and current year

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



#-------------------------------------------------------------------------------------------


relative_output <- rbind(rate_reductions, sum_averted)

fwrite(relative_output, "epi_output/grouped_output/relative_output.csv")



# ---- end


