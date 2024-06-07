# Vaccine Scenario Labelling

# Relabel the values for Incidence and Mortality Rates
epi_099$Indicator  <- ordered(epi_099$Indicator,
                              levels = c("inc_avert", "mort_avert", "inc_RR", "mort_RR"),
                              labels = c("'Cumulative Cases Averted'", 
                                         "'Cumulative Deaths Averted'",
                                         "Incidence Rate Reduction",
                                         "Mortality Rate Reduction"))

epi_099 <- epi_099[grepl("*M72*", epi_099$Runtype), Vaccine := "M72/AS01E scenarios"]
epi_099 <- epi_099[grepl("*BCG*", epi_099$Runtype), Vaccine := "BCG-revaccination scenarios"]
epi_099 <- epi_099[Runtype == "baseline", Vaccine := "No-new-vaccine"]
epi_099$Vaccine <- 
  ordered(epi_099$Vaccine,
          levels = c("M72/AS01E scenarios",
                     "BCG-revaccination scenarios",
                     "No-new-vaccine"))
epi_099$Runtype <- 
  ordered(epi_099$Runtype,
          levels = c("baseline", "M72_AI_POD_50_10yr_med_2030",
                     "BCG_NCI_POI_45_10yr_med_2025", 
                     "M72_AI_POD_60_10yr_med_2030", "M72_AI_POD_70_10yr_med_2030",
                     "M72_AI_POD_50_5yr_med_2030",
                     "M72_AI_POD_50_15yr_med_2030", "M72_AI_POD_50_20yr_med_2030",
                     "M72_AI_POID_50_10yr_med_2030", "M72_CI_POD_50_10yr_med_2030",
                     "BCG_AI_POI_45_10yr_med_2025",
                     "M72_AI_POD_50_10yr_med_2036", 
                     "BCG_NCI_POI_45_10yr_med_2031",
                     "M72_AI_POD_50_10yr_low_2030", "M72_AI_POD_50_10yr_high_2030",
                     "M72_AI_POD_50_10yr_med_2030_diffages",
                     "BCG_NCI_POI_45_10yr_med_2025_diffages",
                     "M72_AI_POD_50_10yr_med_2030_alladults",
                     "BCG_NCI_POI_70_10yr_med_2025",
                     "BCG_NCI_POI_45_5yr_med_2025",
                     "BCG_NCI_POI_45_15yr_med_2025", "BCG_NCI_POI_45_20yr_med_2025",
                     "BCG_NCI_POID_45_10yr_med_2025",
                     "BCG_NCI_POI_45_10yr_low_2025", "BCG_NCI_POI_45_10yr_high_2025",
                     "BCG_NCI_POI_45_10yr_med_2025_alladults"),
          labels = c("No-New-Vaccine", "Basecase (routine age 15, campaign ages 16-34)",
                     "Basecase (routine age 10, campaign ages 11-18)",
                     "60% efficacy", "70% efficacy", 
                     "5 years protection",
                     "15 years protection", "20 years protection",
                     "Prevention of infection and disease",
                     "Efficacy with current infection at vaccination",
                     "Efficacy with any infection at vaccination",
                     "2036 introduction",
                     "2031 introduction",
                     "Lower coverage", "Higher coverage", 
                     "Older Ages (routine age 17, campaign ages 18-55)",
                     "Older Ages (routine age 15, campaign ages 16-34)",
                     "All Adults (routine age 18, campaign ages 19+)",
                     "70% efficacy", 
                     "5 years protection",
                     "15 years protection", "20 years protection",
                     "Prevention of infection and disease", 
                     "Lower coverage", "Higher coverage",
                     "All Adults (routine age 18, campaign ages 19+)"))


epi_099$Vaccine <- ordered(epi_099$Vaccine,
                           levels = c("M72/AS01E scenarios",
                                      "BCG-revaccination scenarios"),
                           labels = c("M72/AS01[E]",
                                      "BCG-revaccination"))

# --- end