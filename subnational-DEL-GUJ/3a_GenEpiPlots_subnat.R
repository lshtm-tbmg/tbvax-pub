#-------------------
# Epi plots
# Rebecca Clark
# Last updated 7 June 2024
#-------------------

# Set-up
suppressPackageStartupMessages({
  rm(list=ls())
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  library(stringr)
  
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
})


# Load in the rate_plots output from GenEpiUncertainty_subnat.R
relative_output_GUJ <- fread("./epi_output/grouped_output/GUJ/relative_output.csv")
raw_output_GUJ      <- fread("./epi_output/grouped_output/GUJ/raw_output.csv")
relative_output_GUJ <- relative_output_GUJ[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]
raw_output_GUJ      <- raw_output_GUJ[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]
epi_estimates_GUJ   <- rbind(relative_output_GUJ, raw_output_GUJ)
epi_estimates_GUJ   <- epi_estimates_GUJ[, Region := "Gujarat"]

relative_output_DEL <- fread("./epi_output/grouped_output/DEL/relative_output.csv")
raw_output_DEL      <- fread("./epi_output/grouped_output/DEL/raw_output.csv")
relative_output_DEL <- relative_output_DEL[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]
raw_output_DEL      <- raw_output_DEL[, .(Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]
epi_estimates_DEL   <- rbind(relative_output_DEL, raw_output_DEL)
epi_estimates_DEL   <- epi_estimates_DEL[, Region := "Delhi"]


epi_099 <- rbind(epi_estimates_DEL, epi_estimates_GUJ)

epi_099 <- epi_099[, .(Region, Year, AgeGrp, Runtype, Indicator, medval, lowval, highval)]

# Subset to All ages and the key variables
epi_099 <- epi_099[AgeGrp == "[0,99]" & (Indicator == "inc_avert" | Indicator == "mort_avert" |
                                           Indicator == "inc_RR" | Indicator == "mort_RR")]

source("./EpiPlots_labels.R")

## Figure 1: Cumulative cases and deaths averted between 2025 and 2050
##for Policy Scenarios for both vaccines and regions. 

ggplot(epi_099[Year == 2050 & grepl("Cumulative", Indicator) &
                 (grepl("Basecase", Runtype) | grepl("ages", Runtype))]) +
  geom_col(aes(x = Runtype, y = medval, fill = Region),
           position = position_dodge(0.7), alpha = 0.5, width = 0.7) +
  geom_errorbar(aes(x = Runtype, ymin = lowval, ymax = highval, col = Region),
                position = position_dodge(width = 0.7), linewidth = 1, width = 0.25) +
  facet_grid(Indicator ~ Vaccine, scales = "free", labeller = label_parsed) +
  theme_minimal_grid() + guides(col = guide_legend(ncol=5)) +
  panel_border(color = "black") + 
  scale_colour_manual(values = c("Delhi" = "#541352FF", "Gujarat" = "#2f9aa0FF")) +
  scale_fill_manual(values = c("Delhi" = "#541352FF", "Gujarat" = "#2f9aa0FF")) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.justification = "right",
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 16),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)
  ) + ylab(label = "Numbers averted (1,000s)")  + 
  xlab(label = "Scenario") +
  scale_y_continuous(limits = c(0, NA), labels = scales::number_format(big.mark = ",")) +
  scale_x_discrete(labels = scales::label_wrap(30))

### ---- end
