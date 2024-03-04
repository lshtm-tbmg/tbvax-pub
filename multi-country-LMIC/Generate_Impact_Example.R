#-------------------
# Generate Impact Output Example
# Updated 4 March 2024
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
  library(arrow)
  library(logger)
  
  source(here("R", "include-v11.R"), model)
  source(here("R", "TBVx-run-v1.R"), model)
  source(here("R", "workflow", "run_param_set_epi_econ.R"))
  
})


# 2. Load in the country code and vx_scenarios file
cc <- "PNG"
print(cc)

# Load in the vaccine scenarios to run
# If you want to run more scenarios, add them to the csv
vxscenarios <- "./processing_files/vx_scenarios.csv"
vx_scenarios <- fread(here("./", vxscenarios))
rr_pct_data <- fread("./processing_files/RRTB_proportions.csv")
rr_pct <- rr_pct_data[country == cc]$rr_pct


# Load in the fitted parameters for the country
parameters <- fread(paste0("./param_sets/", cc, "_params.csv"))

# if you want to subset the parameters, uncomment this
# parameters <- parameters[sample(.N, 200)]


# 3. Make Directories to save output
if (T){
  if(!dir.exists(here("countries", cc, "logs"))) dir.create(here("countries", cc, "logs"), showWarnings = F)
  if(!dir.exists(here("countries", cc, "output"))) dir.create(here("countries", cc, "output"), showWarnings = F)
  
  if(!dir.exists(here("econ_output"))) dir.create(here("econ_output"), showWarnings = F)
  
  if(!dir.exists(here("econ_output/cc_TB"))) dir.create(here("econ_output/cc_TB"), showWarnings = F)
  dir.create(here(paste0("econ_output/cc_TB/", cc, "_TB/")), showWarnings = F)
  
  if(!dir.exists(here("econ_output/cc_TB_HIV"))) dir.create(here("econ_output/cc_TB_HIV"), showWarnings = F)
  dir.create(here(paste0("econ_output/cc_TB_HIV/", cc, "_TB_HIV/")), showWarnings = F)
  
  if(!dir.exists(here("econ_output/cc_alldeaths"))) dir.create(here("econ_output/cc_alldeaths"), showWarnings = F)
  dir.create(here(paste0("econ_output/cc_alldeaths/", cc, "_alldeaths/")), showWarnings = F)
  
  if(!dir.exists(here("epi_output"))) dir.create(here("epi_output"), showWarnings = F)
  if(!dir.exists(here("epi_output/n_epi"))) dir.create(here("epi_output/n_epi"), showWarnings = F)
}

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
    cc_TB_param <- list()
    cc_TB_HIV_param <- list()
    cc_deaths_param <- list()
    
    for (i in 1:nrow(vx_scenarios)){
      
      # subset to the i-th row of characteristics
      vx_chars <- vx_scenarios[i,]
      
      print(paste0("vaccine scenario number ", i))
      
      # run the model with the row of parameters
      vx_scen_output <- run_param_set_epi_econ(model, cc, params, params_uid, vx_chars, rr_pct)
      
      cc_n_epi_param[[i]]  <- vx_scen_output[["n_epi"]]
      cc_TB_param[[i]] <- vx_scen_output[["cc_TB"]]
      cc_TB_HIV_param[[i]] <- vx_scen_output[["cc_TB_HIV"]]
      cc_deaths_param[[i]] <- vx_scen_output[["cc_deaths"]]
      
    }
    
    write_parquet(rbindlist(cc_n_epi_param), file.path("epi_output", "n_epi",
                                                       paste0(cc, "_", params_uid, ".parquet")))
    rm(cc_n_epi_param)
    
    write_parquet(rbindlist(cc_TB_param), file.path("econ_output", "cc_TB", paste0(cc, "_TB"),
                                                    paste0(params_uid, ".parquet")))
    rm(cc_TB_param)
    gc(full=TRUE)
    
    write_parquet(rbindlist(cc_TB_HIV_param), file.path("econ_output", "cc_TB_HIV", paste0(cc, "_TB_HIV"),
                                                        paste0(params_uid, ".parquet")))
    rm(cc_TB_HIV_param)
    gc(full=TRUE)
    
    write_parquet(rbindlist(cc_deaths_param), file.path("econ_output", "cc_alldeaths", paste0(cc, "_alldeaths"),
                                                        paste0(params_uid, ".parquet")))
    rm(cc_deaths_param)
    gc(full=TRUE)
    
    print(paste0("end time for parameter set ", j, " = ", Sys.time()))
    
  }
}


# ---- end



