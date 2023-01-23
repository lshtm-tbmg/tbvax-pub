run_param_set_epi <- function(model, cc, params, params_uid, vx_chars, vx_inc = NA) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 parameters   = vx_chars$input)
  
  # Run the model with the parameter set
  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }
  
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
  
  counts <- output$stocks[,!("RISK")]
  # counts <- counts[grep("count", counts$TB),] # commented out bc TBdead is no longer TBdeadcount
  # counts <- counts[!(year %% 0.5 == 0),] # we only do the .999 years of sim now
  
  counts <- counts[age_from == 0  & age_thru == 14, AgeGrp := "[0,14]"]
  counts <- counts[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  counts <- counts[age_from == 0  & age_thru == 99, AgeGrp := "[0,99]"]
  counts <- counts[, !c("age_from", "age_thru")]
  
  
  #### Incidence
  inc <- counts[TB == "Dscount" & (HIV != "HIVdead"),]
  inc <- inc[, .(N_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  inc <- inc[, Year := floor(Year)]
  
  #### Mortality
  mort <- counts[TB == "TBdead" & (HIV != "HIVdead"),]
  mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  mort <- mort[, Year := floor(Year)]
  
  #### Notifications
  notif <- counts[TB == "DcTcount" & (HIV != "HIVdead"),]
  notif <- notif[, .(N_tx = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  notif <- notif[, Year := floor(Year)]
  
  rm(counts)
  gc(full=TRUE)
  
  ### Population Size
  population <- setDT(output$population)
  population <- population[year >= 2023 & year <= 2051]
  population <- population[!(year %% 0.5 == 0),] # use 0.999
  population <- melt(population, id.vars = c("year", "country"),
                     variable.name = "Age", value.name = "Population")
  pop014 <- population[(Age == 0 | Age == 1 | Age == 2 | Age == 3 | Age == 4 | Age == 5 | 
                          Age == 6 | Age == 7 | Age == 8 | Age == 9 | Age == 10 | 
                          Age == 11 | Age == 12 | Age == 13 | Age == 14)]
  pop014 <- pop014[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  pop014 <- pop014[, AgeGrp := "[0,14]"]
  
  pop1599 <- population[!(Age == 0 | Age == 1 | Age == 2 | Age == 3 | Age == 4 | Age == 5 | 
                            Age == 6 | Age == 7 | Age == 8 | Age == 9 | Age == 10 | 
                            Age == 11 | Age == 12 | Age == 13 | Age == 14)]
  
  pop1599 <- pop1599[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  pop1599 <- pop1599[, AgeGrp := "[15,99]"]
  
  population <- population[, .(Population = sum(Population)), by = .(Country = country, Year = year)]
  population <- population[, AgeGrp := "[0,99]"]
  
  population <- rbind(population, pop1599)
  population <- rbind(population, pop014)
  
  population <- population[, Year := floor(Year)]
  
  # Combine everything into one dataset
  n_epi <- population[inc, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[notif, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  
  rm(population)
  rm(inc)
  rm(mort)
  rm(notif)
  
  if (vx_chars$runtype == "baseline"){
    model$baseline_output <- output
  }
  
  rm(output)
  gc(full=TRUE)
  
  combined_ipj[["n_epi"]] <- n_epi[, `:=`(UID = params_uid,
                                          Runtype = vx_chars$runtype,
                                          Scenario = vx_chars$scenario)]
  
  combined_ipj
}
