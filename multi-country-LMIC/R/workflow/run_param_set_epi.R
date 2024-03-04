run_param_set_epi <- function(model, cc, params, params_uid, vx_chars) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- model$set.paths(countrycode  = cc,
                                 xml          = vx_chars$xml,
                                 parameters   = vx_chars$input)
  

  if (vx_chars$runtype != "baseline"){
    scen_baseline = model$baseline_output 
  } else if (vx_chars$runtype == "baseline"){
    scen_baseline = NULL
  }

  # Run the model with the parameter set
  output = model$run(model, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
  
  # Start processing the output
  counts <- output$stocks[,!("RISK")]
  counts <- counts[grep("count", counts$TB),]
  counts <- counts[!(year %% 0.5 == 0),]
  counts <- counts[age_from == 0  & age_thru == 14, AgeGrp := "[0,14]"]
  counts <- counts[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  counts <- counts[age_from == 0  & age_thru == 99, AgeGrp := "[0,99]"]
  counts <- counts[, !c("age_from", "age_thru")]
  
  #### Number of TB cases per year (incidence)
  inc <- counts[TB == "Dscount" & (HIV != "HIVdead"),]
  inc <- inc[, .(N_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  inc <- inc[, Year := floor(Year)]
  
  #### Number of treatment initiations per year (case notifications)
  notif <- counts[TB == "DcTcount" & (HIV != "HIVdead"),]
  notif <- notif[, .(N_tx = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  notif <- notif[, Year := floor(Year)]
  
  #### Number of deaths per year (mortality)
  mort <- output$stocks[,!("RISK")]
  mort <- mort[TB == "TBdead" & (HIV != "HIVdead"),]
  mort <- mort[!(grep("count", mort$VXa)),]
  mort <- mort[!(year %% 0.5 == 0),]
  mort <- mort[age_from == 0  & age_thru == 14, AgeGrp := "[0,14]"]
  mort <- mort[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  mort <- mort[age_from == 0  & age_thru == 99, AgeGrp := "[0,99]"]
  mort <- mort[, !c("age_from", "age_thru")]
  
  mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  mort <- mort[, Year := floor(Year)]
  
  rm(counts)
  gc(full=TRUE)
  
  
  #### Number of vaccines delivered per year
  tb_vxa <- output$stocks[,!("RISK")]
  tb_vxa <- tb_vxa[grep("count", tb_vxa$VXa),]
  tb_vxa <- tb_vxa[!(year %% 0.5 == 0),]
  
  tb_vxa <- tb_vxa[age_from == 0  & age_thru == 14, AgeGrp := "[0,14]"]
  tb_vxa <- tb_vxa[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  tb_vxa <- tb_vxa[age_from == 0  & age_thru == 99, AgeGrp := "[0,99]"]
  tb_vxa <- tb_vxa[, !c("age_from", "age_thru")]
  
  tb_vxa <- tb_vxa[(VXa == "vaccount" | VXa == "prevcount" | VXa == "recvcount") & TB != "Rdead" & TB!= "TBdead" & HIV != "HIVdead"]
  tb_vxa <- tb_vxa[,.(N_vac = abs(sum(value))), by = .(Country = country, Year = year, AgeGrp)]
  tb_vxa <- tb_vxa[, Year := floor(Year)]
  
  
  # Put everything together
  n_epi <- inc[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[notif, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  
  n_epi <- n_epi[Year < 2051]
  
  n_epi <- n_epi[tb_vxa, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  
  rm(inc)
  rm(mort)
  rm(notif)
  rm(tb_vxa)
  
  if (vx_chars$runtype == "baseline"){
    model$baseline_output <- output
  }
  
  rm(output)
  gc(full=TRUE)
  
  combined_ipj[["n_epi"]] <- n_epi[, `:=`(UID = params_uid,
                                          Runtype = vx_chars$runtype)]
  
  combined_ipj
  
}