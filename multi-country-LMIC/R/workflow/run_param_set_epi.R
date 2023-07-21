run_param_set_epi <- function(model, cc, params, params_uid, vx_chars) {
  
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
  
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline)
  
  if (length(output$stocks) != 0) {
    
    counts <- output$stocks[,!("RISK")]
    counts <- counts[grep("count", counts$TB),]
    counts <- counts[!(year %% 0.5 == 0),]

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
    
    n_epi <- inc[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[notif, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    
    n_epi <- n_epi[Year >= 2024]
    
    rm(inc)
    rm(mort)
    rm(notif)
    
    if (vx_chars$runtype == "baseline"){
      model$baseline_output <- output
    }
    
    rm(output)
    gc(full=TRUE)
    
    combined_ipj[["n_epi"]] <- n_epi[, `:=`(UID = params_uid,
                                            Runtype = vx_chars$runtype)]
    
    combined_ipj
  }
}