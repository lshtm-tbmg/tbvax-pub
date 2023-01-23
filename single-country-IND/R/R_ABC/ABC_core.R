# Model
# function takes one argument (vector of params) and outputs one vector of summary stats
run.one.abc = function(inputPoints){
  
  # update try counter
  tc <<- tc + 1
  log_debug("TRY COUNTER: %s", tc)
  
  
  p_values        <- as.data.frame(as.list(inputPoints))
  names(p_values) <- init.names       # reassign the names because they don't come through with inputPoints
  unique_id       <- digest(p_values) # give each param set a unique id

  
  # Run the model with the parameter set
  output = run(model, new.parameter.values = p_values)
  
  
  nhits <- sum(output[["hits"]]$fit)
  
  ## OLD

  #  update XML with proposed inputPoints
  # p_values        <- as.data.frame(as.list(inputPoints))
  # names(p_values) <- init.names       # reassign the names because they don't come through with inputPoints
  # unique_id       <- digest(p_values) # give each param set a unique id
  
  # param.abc          <- param.data[param.data$unique.name %in% names(p_values[1,]),] 
  # read_params        <- params#read.model.parameters(params)#h
  # updated_params     <- update.model.parameters(read_params, p_values)#h
  
  #reinitialize with the new params
  #initialized_params <- initialize.model.parameters(updated_params) #h
  # run the model with the new parameters
  # output             <- run.model(initialized_params)#h
  
  
  #use the function to get target hits and and calculate number of hits
  # out <- get.target.hits(output, initialized_params)
  # nhits <- sum(out$hits$fit)
  
  log_info("Number of hits: %s, previous max hits: %s", nhits, max_hits)
  
  # logging if we've increased the maximum num of hits
  if (nhits > max_hits) {
    log_info("------------ MAX HIT INCREASE --------: %s -> %s", max_hits, nhits)
    max_hits <<- nhits
  } else if (nhits == target & seed_search == TRUE) {
    log_info("SEED SEARCH: %s", nhits)
  }
  
  
  #Identify what to write out
  hitarray[[gc]] <<- c(list(total = nhits, task = grid_task_int, unique_id = unique_id),p_values)
  
  
  if (hitarray[[gc]]$total >= target) {
    ahitarray[[gc]] <<- c(list(total = nhits, task = grid_task_int, unique_id = unique_id), p_values)
    ret_int <- 1
  } else {
    ret_int <- 0
  }
  log_debug("GC: %s", gc)
  
  
  
  if (gc%%wo_interval == 0 | gc == n.samp) {
    hitarray <<- rbindlist(hitarray[!sapply(hitarray, is.null)])
    fwrite(hitarray, paste0(paths$output.dir,"/",RUNID_TS,"_", grid_task_int, "_run_hit_table.csv"),
           col.names = FALSE, sep = ",", append = TRUE, row.names = FALSE, logical01 = T)
    
    if (length(ahitarray) >= 1) {
      ahitarray <<- rbindlist(ahitarray[!sapply(ahitarray, is.null)])
      fwrite(ahitarray, paste0(paths$output.dir,"/", RUNID_TS, "_", grid_task_int, "_run_ahit_table.csv"),
             col.names = FALSE, sep = ",", append = TRUE, row.names = FALSE, logical01 = T)
    }
    
    # Reset matrices
    hitarray <<- list()
    ahitarray <<- list()
    log_info("Chain length / Write out: %s >> Try length: %s >> G/T: %3.2f", gc, tc, gc/tc)
    
  }
  
  # update the global counter
  gc <<- gc + 1
  
  
  # Return the target summary statistic
  if (ret_int == 1) {
    rm(ret_int)
    return(0.5)
  } else if (ret_int == 0) {
    return(5)
  }
  
  
}