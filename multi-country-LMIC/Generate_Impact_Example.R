#-------------------
# Generate Impact Output Example
# 21 July 2023
# Rebecca Clark
#-------------------

rm(list=ls())

# 1. Set-up
suppressPackageStartupMessages({
  model = new.env()
  library(here)
  library(data.table)
  library(renv)
  library(digest)
  library(log4r)
  library(git2r)
  library(arrow)
  library(logger)
  
  source(here("R", "include-v11.R"), model)
  source(here("R", "TBVx-run-v1.R"), model)
  source(here("R", "workflow", "run_param_set_epi.R"))
  
})


# 2. Load in the country code and vx_scenarios file
cc <- "IDN"
print(cc)

# Load in the vaccine scenarios to run
vxscenarios <- "epi_vx_scenarios.csv"
vx_scenarios <- fread(here("./", vxscenarios))


# Load in the fitted parameters for the country
parameters <- fread(paste0("./param_sets/", cc, "_params.csv"))



# 3. Make Directories to save output
if(!dir.exists(here("epi_output"))) dir.create(here("epi_output"), showWarnings = F)
if(!dir.exists(here("epi_output/n_epi"))) dir.create(here("epi_output/n_epi"), showWarnings = F)


# 4. Generate the output and write out 
for (j in 1:nrow(parameters)){
  
  print(paste0("start time = ", Sys.time()))
  
  print(paste0("starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  if (paste0(cc, "_", params_uid, ".parquet") %in% list.files("epi_output/n_epi/")){
    
    print("Already done parameter set!")
    
  } else {
    
    # Get rid of everything except parameters
    params <- params[, !c("uid", "n_hit")]
    params <- unlist(params)
    
    # Run through all the vaccine scenarios for each parameter set
    cc_n_epi_param <- list()
    
    for (i in 1:nrow(vx_scenarios)){
      
      # subset to the i-th row of characteristics
      vx_chars <- vx_scenarios[i,]
      
      print(paste0("vaccine scenario number ", i))
      
      # run the model with the row of parameters
      vx_scen_output <- run_param_set_epi(model, cc, params, params_uid, vx_chars)
      
      cc_n_epi_param[[i]]  <- vx_scen_output[["n_epi"]]
      
    }
    
    write_parquet(rbindlist(cc_n_epi_param), file.path("epi_output", "n_epi",
                                                       paste0(cc, "_", params_uid, ".parquet")))
    rm(cc_n_epi_param)
    
    print(paste0("end time for parameter set ", j, " = ", Sys.time()))
    
  }
}




# ---- end









