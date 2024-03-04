#-------------------
# Example plots
# Rebecca Clark
# Updated 4 March 2024
#-------------------

suppressPackageStartupMessages({
  rm(list=ls())
  library(rlang)
  library(fs)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
})

# Plot of cumulative number of cases, treatments, deaths
sum_cumulative <- fread("epi_output/grouped_output/cumulative_numbers_output.csv")

sum_cumulative$Runtype <- factor(sum_cumulative$Runtype,
                                 levels = c("baseline", "infant_vaccine", "adult_vaccine"),
                                 labels = c("No-new-vaccine", "Infant vaccine", "Adolescent/adult vaccine"))

sum_cumulative$Indicator <- factor(sum_cumulative$Indicator,
                                   levels = c("sum_inc", "sum_tx", "sum_mort"),
                                   labels = c("TB cases", "TB treatment notifications", "TB deaths"))

ggplot(data = sum_cumulative) +
  geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval, col = Runtype)) +
  scale_colour_viridis_d(direction = 1, option = "viridis") +
  scale_fill_viridis_d(direction = 1, option = "viridis") +
  ylab("Cumulative number (1000s)") + ylim(c(0,NA)) +
  facet_wrap( ~ Indicator) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(size = 14)
  ) + labs(title = "Cumulative number from 2025")

# Plot of cumulative number of AVERTED cases, treatments, deaths
sum_averted <- fread("./epi_output/grouped_output/cumulative_numbers_averted_output.csv")
sum_averted$Runtype <- factor(sum_averted$Runtype,
                              levels = c("baseline", "infant_vaccine", "adult_vaccine"),
                              labels = c("No-new-vaccine", "Infant vaccine", "Adolescent/adult vaccine"))

sum_averted$Indicator <- factor(sum_averted$Indicator,
                                levels = c("inc_avert", "tx_avert", "mort_avert"),
                                labels = c("TB cases", "TB treatment notifications", "TB deaths"))

ggplot(data = sum_averted) +
  geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval, col = Runtype)) +
  scale_colour_viridis_d(direction = 1, option = "viridis") +
  scale_fill_viridis_d(direction = 1, option = "viridis") +
  ylab("Cumulative number averted (1000s)") + ylim(c(0,NA)) +
  facet_wrap( ~ Indicator) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(size = 14)
  ) + labs(title = "Cumulative number averted from 2025")

# ---- end

