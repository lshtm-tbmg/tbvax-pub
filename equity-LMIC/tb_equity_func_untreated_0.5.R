index<-as.integer(commandArgs(trailingOnly=TRUE)[1])
tbequity_0.5 <- function(index){

## Load packages
library(dplyr)
library(tidyr)
library(data.table)
library(arrow)
library(magrittr)
library(stringr)
library(reshape2)

## Create lists of countries
countries <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/LMIC_emulation_revised.csv",header=TRUE,check.names=FALSE)[,2]
base <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/LMIC_emulation_revised.csv",header=TRUE,check.names=FALSE)
  
## Specify discount rate & analytic years
r <- 0.03 #discount rate
disc_year <- 2028 #first year of vaccine introduction
start_year <- 2024 #first year of analytic results
end_year <- 2050
num_years <- length(seq(start_year,2050,1)) #time horizon
N <- 1000 #number of parameter sets
untreated_mult <- 0.5

## Load equity inputs
ses <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/ses_targets.csv",header=TRUE,check.names=FALSE)
care <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/careseeking_pred.csv",header=TRUE,check.names=FALSE)
prop <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/ses_proportions.csv",header=TRUE,check.names=FALSE)

## Convert equity inputs to long format
ses_long <- pivot_longer(data=ses,cols=3:7,names_to="quintile",values_to="ses")
care_long <- pivot_longer(data=care,cols=5:9,names_to="quintile",values_to="careseeking")
prop_long <- pivot_longer(data=prop,cols=3:7,names_to="quintile",values_to="prop")

## Load cost uncertainty sets

poorest_pred_dm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_dm_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)
poorest_pred_nm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_nm_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)
poorest_pred_ind <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_ind_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)

poorer_pred_dm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_dm_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)
poorer_pred_nm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_nm_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)
poorer_pred_ind <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_ind_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)

middle_pred_dm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_dm_middle_pred_sets.csv",header=TRUE,check.names=FALSE)
middle_pred_nm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_nm_middle_pred_sets.csv",header=TRUE,check.names=FALSE)
middle_pred_ind <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_ind_middle_pred_sets.csv",header=TRUE,check.names=FALSE)

richer_pred_dm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_dm_richer_pred_sets.csv",header=TRUE,check.names=FALSE)
richer_pred_nm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_nm_richer_pred_sets.csv",header=TRUE,check.names=FALSE)
richer_pred_ind <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_ind_richer_pred_sets.csv",header=TRUE,check.names=FALSE)

richest_pred_dm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_dm_richest_pred_sets.csv",header=TRUE,check.names=FALSE)
richest_pred_nm <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_nm_richest_pred_sets.csv",header=TRUE,check.names=FALSE)
richest_pred_ind <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cost_ind_richest_pred_sets.csv",header=TRUE,check.names=FALSE)

## Load catastrophic cost proportion uncertainty sets
cc_all <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cc_all.csv",header=TRUE,check.names=FALSE)
oop_all <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/oop_all.csv",header=TRUE,check.names=FALSE)
cc_all_0.5 <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/cc_all_0.5.csv",header=TRUE,check.names=FALSE)
oop_all_0.5 <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/oop_all_0.5.csv",header=TRUE,check.names=FALSE)

# outermost loop = country

i <- countries[index]
  
  # Initialize dataframe
  output_year <- as.data.frame(seq(start_year,end_year,1))
  colnames(output_year) <- "year"
  output_all <- NULL
  
  ## Grab list of UIDs
  directory <- paste0("/n/holyscratch01/menzies_lab/Lab/EconOutputRedo/tbvax/econ_output/")
  
  UID_list <- list.files(path = paste0(directory,"cc_TB/",i,"_TB/"))
  UID_list <- tools::file_path_sans_ext(UID_list)
  
  # Set econ parameter set counter to zero
  
  counter <- 0
  
# second loop: uid

  for (j in UID_list) {
    
    ## Load data
    cty <- read_parquet(paste0(directory,"cc_TB/",i,"_TB/",j,".parquet")) %>% collect()
    base_cty <- base[which(base$code==i),]
    
    ## Subset equity inputs to country
    ses_long_cty <- ses_long[which(ses_long$code==i),]
    care_long_cty <- care_long[which(care_long$code==i),]
    prop_long_cty <- prop_long[which(prop_long$code==i),]
    
    ## Subset costs/income to specified country
    poorest_dm_cty <- poorest_pred_dm[which(poorest_pred_dm$iso3==i),]
    poorest_nm_cty <- poorest_pred_nm[which(poorest_pred_nm$iso3==i),]
    poorest_ind_cty <- poorest_pred_ind[which(poorest_pred_ind$iso3==i),]
 
    poorer_dm_cty <- poorer_pred_dm[which(poorer_pred_dm$iso3==i),]
    poorer_nm_cty <- poorer_pred_nm[which(poorer_pred_nm$iso3==i),]
    poorer_ind_cty <- poorer_pred_ind[which(poorer_pred_ind$iso3==i),]

    middle_dm_cty <- middle_pred_dm[which(middle_pred_dm$iso3==i),]
    middle_nm_cty <- middle_pred_nm[which(middle_pred_nm$iso3==i),]
    middle_ind_cty <- middle_pred_ind[which(middle_pred_ind$iso3==i),]
   
    richer_dm_cty <- richer_pred_dm[which(richer_pred_dm$iso3==i),]
    richer_nm_cty <- richer_pred_nm[which(richer_pred_nm$iso3==i),]
    richer_ind_cty <- richer_pred_ind[which(richer_pred_ind$iso3==i),]
   
    richest_dm_cty <- richest_pred_dm[which(richest_pred_dm$iso3==i),]
    richest_nm_cty <- richest_pred_nm[which(richest_pred_nm$iso3==i),]
    richest_ind_cty <- richest_pred_ind[which(richest_pred_ind$iso3==i),]
    
    ## Subset cc/oop to specified country
    cc_cty <- cc_all[which(cc_all$code==i),]
    oop_cty <- oop_all[which(oop_all$code==i),]
    cc_cty_0.5 <- cc_all_0.5[which(cc_all_0.5$code==i),]
    oop_cty_0.5 <- oop_all_0.5[which(oop_all_0.5$code==i),]
    
    ## Reformat variables
    # cty$Year <- as.numeric(cty$Year - 0.5)
    
    # The following backs out numeric values for the first age shown (Age) in AgeGrp, 
    # regexpr() gives the ctyex of given characters, 
    # and substr() extracts a piece of the character vector. 
    
    cty$Age <- as.numeric(substr(cty$AgeGrp,2,regexpr(",",cty$AgeGrp)-1))
    
    # Grab list of delivery scenarios
    
    scenarios <- c("baseline","basecase_med_10y_PRI_Neo","basecase_med_10y_PPI_AdoAdu")
    
    # Increase econ parameter set counter
    
    counter <- counter + 1
    
# third loop: delivery scenario
# calc all results then separate step for summary statistics
    
    for (k in scenarios) {
      
      inc_scen <- cty[which(cty$Runtype==k&cty$Year>=start_year&cty$Year<=end_year),]
      
      ## Aggregate population by scenario
      output_year$pop <- aggregate(inc_scen$Population~inc_scen$Year,FUN=sum)[,2]
      
      ## Aggregate cases by scenario, SES stratified
      cases_SES_high <- aggregate(inc_scen$SES_High_Inc~inc_scen$Year,FUN=sum)[,2]
      cases_SES_low <- aggregate(inc_scen$SES_Low_Inc~inc_scen$Year,FUN=sum)[,2]
      
      ## Aggregate treated cases by scenario, SES stratified
      notif_SES_high <- aggregate(inc_scen$SES_High_Notif~inc_scen$Year,FUN=sum)[,2]
      notif_SES_low <- aggregate(inc_scen$SES_Low_Notif~inc_scen$Year,FUN=sum)[,2]
      
      ## Calculate untreated cases by scenario by SES stratum
      untreated_SES_high <- cases_SES_high - notif_SES_high
      untreated_SES_low <- cases_SES_low - notif_SES_low
      
      ## Extrapolate cases by two SES strata to five income quintiles by SES prevalence rate ratio
      output_year$cases_richest <- cases_SES_high*(ses_long_cty$ses[5]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]))*1000
      output_year$cases_richer <- cases_SES_high*(ses_long_cty$ses[4]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]))*1000
      output_year$cases_middle <- cases_SES_high*(ses_long_cty$ses[3]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]))*1000
      output_year$cases_poorer <- cases_SES_low*(ses_long_cty$ses[2]/(ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      output_year$cases_poorest <- cases_SES_low*(ses_long_cty$ses[1]/(ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      
      ## Estimate p(treatment) by two SES strata
      p_tx_high <- notif_SES_high / cases_SES_high
      p_tx_low <- notif_SES_low / cases_SES_low
      
      ## Extrapolate treated by two SES strata to five income quintiles by SES prevalence rate ratio and DHS care-seeking
      notif_richest <- cases_SES_high*p_tx_high*((care_long_cty$careseeking[5]*output_year$cases_richest)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle))*1000
      notif_richer <- cases_SES_high*p_tx_high*((care_long_cty$careseeking[4]*output_year$cases_richer)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle))*1000
      notif_middle <- cases_SES_high*p_tx_high*((care_long_cty$careseeking[3]*output_year$cases_middle)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle))*1000
      notif_poorer <- cases_SES_low*p_tx_low*((care_long_cty$careseeking[2]*output_year$cases_poorer)/(care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      notif_poorest <- cases_SES_low*p_tx_low*((care_long_cty$careseeking[1]*output_year$cases_poorest)/(care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      
      ## Constrain so treated/notif is always less than or equal to total TB cases 
      notif_richest <- pmin(output_year$cases_richest,notif_richest)
      notif_richer <- pmin(output_year$cases_richer,notif_richer)
      notif_middle <- pmin(output_year$cases_middle,notif_middle)
      notif_poorer <- pmin(output_year$cases_poorer,notif_poorer)
      notif_poorest <- pmin(output_year$cases_poorest,notif_poorest)
      
      ## Calculate untreated by quintile
      untreated_richest <- output_year$cases_richest - notif_richest
      untreated_richer <- output_year$cases_richer - notif_richer
      untreated_middle <- output_year$cases_middle - notif_middle
      untreated_poorer <- output_year$cases_poorer - notif_poorer
      untreated_poorest <- output_year$cases_poorest - notif_poorest
      
      ## Adjust out-of-pocket costs from TB quintiles in PCS to population quintiles
      v1 <- c(prop_long_cty$prop[1],prop_long_cty$prop[2],prop_long_cty$prop[3],prop_long_cty$prop[4],prop_long_cty$prop[5])
      id <- round(quantile(0:1e5,c(0,cumsum(v1))))
      
      # direct medical costs
      dm1 <- c(poorest_dm_cty[,counter+4],poorer_dm_cty[,counter+4],middle_dm_cty[,counter+4],richer_dm_cty[,counter+4],richest_dm_cty[,counter+4])
      dm2 <- rep(NA,5)
      v2 <- rep(dm1,each=1e5/5)
      for(l in 1:5) dm2[l] <- mean(v2[id[l]:id[l+1]])
      
      # direct non-medical costs
      nm1 <- c(poorest_nm_cty[,counter+4],poorer_nm_cty[,counter+4],middle_nm_cty[,counter+4],richer_nm_cty[,counter+4],richest_nm_cty[,counter+4])
      nm2 <- rep(NA,5)
      v3 <- rep(nm1,each=1e5/5)
      for(l in 1:5) nm2[l] <- mean(v3[id[l]:id[l+1]])
      
      # indirect costs
      ind1 <- c(poorest_ind_cty[,counter+4],poorer_ind_cty[,counter+4],middle_ind_cty[,counter+4],richer_ind_cty[,counter+4],richest_ind_cty[,counter+4])
      ind2 <- rep(NA,5)
      v4 <- rep(ind1,each=1e5/5)
      for(l in 1:5) ind2[l] <- mean(v4[id[l]:id[l+1]])
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile among treated cases
      dm_treated_richest_und <- dm2[5] * notif_richest
      dm_treated_richer_und <- dm2[4] * notif_richer
      dm_treated_middle_und <- dm2[3] * notif_middle
      dm_treated_poorer_und <- dm2[2] * notif_poorer
      dm_treated_poorest_und <- dm2[1] * notif_poorest
      
      ## Calculate direct non-medical costs by scenario and income quintile among treated cases
      nm_treated_richest_und <- nm2[5] * notif_richest
      nm_treated_richer_und <- nm2[4] * notif_richer
      nm_treated_middle_und <- nm2[3] * notif_middle
      nm_treated_poorer_und <- nm2[2] * notif_poorer
      nm_treated_poorest_und <- nm2[1] * notif_poorest
      
      ## Calculate indirect costs by scenario and income quintile among treated cases
      ind_treated_richest_und <- ind2[5] * notif_richest
      ind_treated_richer_und <- ind2[4] * notif_richer
      ind_treated_middle_und <- ind2[3] * notif_middle
      ind_treated_poorer_und <- ind2[2] * notif_poorer
      ind_treated_poorest_und <- ind2[1] * notif_poorest
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile among treated cases
      total_treated_richest_und <- (dm2[5]+nm2[5]+ind2[5]) * notif_richest
      total_treated_richer_und <- (dm2[4]+nm2[4]+ind2[4]) * notif_richer
      total_treated_middle_und <- (dm2[3]+nm2[3]+ind2[3]) * notif_middle
      total_treated_poorer_und <- (dm2[2]+nm2[2]+ind2[2]) * notif_poorer
      total_treated_poorest_und <- (dm2[1]+nm2[1]+ind2[1]) * notif_poorest
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile among untreated cases
      dm_untreated_richest_und <- dm2[5] * untreated_mult * untreated_richest
      dm_untreated_richer_und <- dm2[4] * untreated_mult * untreated_richer
      dm_untreated_middle_und <- dm2[3] * untreated_mult * untreated_middle
      dm_untreated_poorer_und <- dm2[2] * untreated_mult * untreated_poorer
      dm_untreated_poorest_und <- dm2[1] * untreated_mult * untreated_poorest
      
      ## Calculate direct non-medical costs by scenario and income quintile among untreated cases
      nm_untreated_richest_und <- nm2[5] * untreated_mult * untreated_richest
      nm_untreated_richer_und <- nm2[4] * untreated_mult * untreated_richer
      nm_untreated_middle_und <- nm2[3] * untreated_mult * untreated_middle
      nm_untreated_poorer_und <- nm2[2] * untreated_mult * untreated_poorer
      nm_untreated_poorest_und <- nm2[1] * untreated_mult * untreated_poorest
      
      ## Calculate indirect costs by scenario and income quintile among untreated cases
      ind_untreated_richest_und <- ind2[5] * untreated_mult * untreated_richest
      ind_untreated_richer_und <- ind2[4] * untreated_mult * untreated_richer
      ind_untreated_middle_und <- ind2[3] * untreated_mult * untreated_middle
      ind_untreated_poorer_und <- ind2[2] * untreated_mult * untreated_poorer
      ind_untreated_poorest_und <- ind2[1] * untreated_mult * untreated_poorest
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile among untreated cases
      total_untreated_richest_und <- ((dm2[5]+nm2[5]+ind2[5])* untreated_mult) * untreated_richest
      total_untreated_richer_und <- ((dm2[4]+nm2[4]+ind2[4])* untreated_mult) * untreated_richer
      total_untreated_middle_und <- ((dm2[3]+nm2[3]+ind2[3])* untreated_mult) * untreated_middle
      total_untreated_poorer_und <- ((dm2[2]+nm2[2]+ind2[2])* untreated_mult) * untreated_poorer
      total_untreated_poorest_und <- ((dm2[1]+nm2[1]+ind2[1])* untreated_mult) * untreated_poorest
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile
      output_year$dm_richest_und <- dm_treated_richest_und + dm_untreated_richest_und
      output_year$dm_richer_und <- dm_treated_richer_und + dm_untreated_richer_und
      output_year$dm_middle_und <- dm_treated_middle_und + dm_untreated_middle_und
      output_year$dm_poorer_und <- dm_treated_poorer_und + dm_untreated_poorer_und
      output_year$dm_poorest_und <- dm_treated_poorest_und + dm_untreated_poorest_und
      
      ## Calculate direct non-medical costs by scenario and income quintile
      output_year$nm_richest_und <- nm_treated_richest_und + nm_untreated_richest_und
      output_year$nm_richer_und <- nm_treated_richer_und + nm_untreated_richer_und
      output_year$nm_middle_und <- nm_treated_middle_und + nm_untreated_middle_und
      output_year$nm_poorer_und <- nm_treated_poorer_und + nm_untreated_poorer_und
      output_year$nm_poorest_und <- nm_treated_poorest_und + nm_untreated_poorest_und
      
      ## Calculate indirect costs by scenario and income quintile
      output_year$ind_richest_und <- ind_treated_richest_und + ind_untreated_richest_und
      output_year$ind_richer_und <- ind_treated_richer_und + ind_untreated_richer_und
      output_year$ind_middle_und <- ind_treated_middle_und + ind_untreated_middle_und
      output_year$ind_poorer_und <- ind_treated_poorer_und + ind_untreated_poorer_und
      output_year$ind_poorest_und <- ind_treated_poorest_und + ind_untreated_poorest_und
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile
      output_year$total_richest_und <- total_treated_richest_und + total_untreated_richest_und
      output_year$total_richer_und <- total_treated_richer_und + total_untreated_richer_und
      output_year$total_middle_und <- total_treated_middle_und + total_untreated_middle_und
      output_year$total_poorer_und <- total_treated_poorer_und + total_untreated_poorer_und
      output_year$total_poorest_und <- total_treated_poorest_und + total_untreated_poorest_und
      
      # ## Estimate discounted and undiscounted OOP costs
      # for (m in 1:nrow(output_year)) {
      #   
      #   ## Treated cases
      #   
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_treated_richest_d"] <- output_year[m,"oop_treated_richest_und"]
      #   } else {
      #     output_year[m,"oop_treated_richest_d"] <- output_year[m,"oop_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_treated_richer_d"] <- output_year[m,"oop_treated_richer_und"]
      #   } else {
      #     output_year[m,"oop_treated_richer_d"] <- output_year[m,"oop_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_treated_middle_d"] <- output_year[m,"oop_treated_middle_und"]
      #   } else {
      #     output_year[m,"oop_treated_middle_d"] <- output_year[m,"oop_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_treated_poorer_d"] <- output_year[m,"oop_treated_poorer_und"]
      #   } else {
      #     output_year[m,"oop_treated_poorer_d"] <- output_year[m,"oop_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_treated_poorest_d"] <- output_year[m,"oop_treated_poorest_und"]
      #   } else {
      #     output_year[m,"oop_treated_poorest_d"] <- output_year[m,"oop_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_treated_richest_d"] <- output_year[m,"nm_treated_richest_und"]
      #   } else {
      #     output_year[m,"nm_treated_richest_d"] <- output_year[m,"nm_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_treated_richer_d"] <- output_year[m,"nm_treated_richer_und"]
      #   } else {
      #     output_year[m,"nm_treated_richer_d"] <- output_year[m,"nm_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_treated_middle_d"] <- output_year[m,"nm_treated_middle_und"]
      #   } else {
      #     output_year[m,"nm_treated_middle_d"] <- output_year[m,"nm_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_treated_poorer_d"] <- output_year[m,"nm_treated_poorer_und"]
      #   } else {
      #     output_year[m,"nm_treated_poorer_d"] <- output_year[m,"nm_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_treated_poorest_d"] <- output_year[m,"nm_treated_poorest_und"]
      #   } else {
      #     output_year[m,"nm_treated_poorest_d"] <- output_year[m,"nm_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_treated_richest_d"] <- output_year[m,"ind_treated_richest_und"]
      #   } else {
      #     output_year[m,"ind_treated_richest_d"] <- output_year[m,"ind_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_treated_richer_d"] <- output_year[m,"ind_treated_richer_und"]
      #   } else {
      #     output_year[m,"ind_treated_richer_d"] <- output_year[m,"ind_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_treated_middle_d"] <- output_year[m,"ind_treated_middle_und"]
      #   } else {
      #     output_year[m,"ind_treated_middle_d"] <- output_year[m,"ind_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_treated_poorer_d"] <- output_year[m,"ind_treated_poorer_und"]
      #   } else {
      #     output_year[m,"ind_treated_poorer_d"] <- output_year[m,"ind_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_treated_poorest_d"] <- output_year[m,"ind_treated_poorest_und"]
      #   } else {
      #     output_year[m,"ind_treated_poorest_d"] <- output_year[m,"ind_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_treated_richest_d"] <- output_year[m,"total_treated_richest_und"]
      #   } else {
      #     output_year[m,"total_treated_richest_d"] <- output_year[m,"total_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_treated_richer_d"] <- output_year[m,"total_treated_richer_und"]
      #   } else {
      #     output_year[m,"total_treated_richer_d"] <- output_year[m,"total_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_treated_middle_d"] <- output_year[m,"total_treated_middle_und"]
      #   } else {
      #     output_year[m,"total_treated_middle_d"] <- output_year[m,"total_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_treated_poorer_d"] <- output_year[m,"total_treated_poorer_und"]
      #   } else {
      #     output_year[m,"total_treated_poorer_d"] <- output_year[m,"total_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_treated_poorest_d"] <- output_year[m,"total_treated_poorest_und"]
      #   } else {
      #     output_year[m,"total_treated_poorest_d"] <- output_year[m,"total_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   
      #   ## Untreated cases
      #   
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_untreated_richest_d"] <- output_year[m,"oop_untreated_richest_und"]
      #   } else {
      #     output_year[m,"oop_untreated_richest_d"] <- output_year[m,"oop_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_untreated_richer_d"] <- output_year[m,"oop_untreated_richer_und"]
      #   } else {
      #     output_year[m,"oop_untreated_richer_d"] <- output_year[m,"oop_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_untreated_middle_d"] <- output_year[m,"oop_untreated_middle_und"]
      #   } else {
      #     output_year[m,"oop_untreated_middle_d"] <- output_year[m,"oop_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_untreated_poorer_d"] <- output_year[m,"oop_untreated_poorer_und"]
      #   } else {
      #     output_year[m,"oop_untreated_poorer_d"] <- output_year[m,"oop_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_untreated_poorest_d"] <- output_year[m,"oop_untreated_poorest_und"]
      #   } else {
      #     output_year[m,"oop_untreated_poorest_d"] <- output_year[m,"oop_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_untreated_richest_d"] <- output_year[m,"nm_untreated_richest_und"]
      #   } else {
      #     output_year[m,"nm_untreated_richest_d"] <- output_year[m,"nm_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_untreated_richer_d"] <- output_year[m,"nm_untreated_richer_und"]
      #   } else {
      #     output_year[m,"nm_untreated_richer_d"] <- output_year[m,"nm_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_untreated_middle_d"] <- output_year[m,"nm_untreated_middle_und"]
      #   } else {
      #     output_year[m,"nm_untreated_middle_d"] <- output_year[m,"nm_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_untreated_poorer_d"] <- output_year[m,"nm_untreated_poorer_und"]
      #   } else {
      #     output_year[m,"nm_untreated_poorer_d"] <- output_year[m,"nm_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_untreated_poorest_d"] <- output_year[m,"nm_untreated_poorest_und"]
      #   } else {
      #     output_year[m,"nm_untreated_poorest_d"] <- output_year[m,"nm_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_untreated_richest_d"] <- output_year[m,"ind_untreated_richest_und"]
      #   } else {
      #     output_year[m,"ind_untreated_richest_d"] <- output_year[m,"ind_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_untreated_richer_d"] <- output_year[m,"ind_untreated_richer_und"]
      #   } else {
      #     output_year[m,"ind_untreated_richer_d"] <- output_year[m,"ind_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_untreated_middle_d"] <- output_year[m,"ind_untreated_middle_und"]
      #   } else {
      #     output_year[m,"ind_untreated_middle_d"] <- output_year[m,"ind_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_untreated_poorer_d"] <- output_year[m,"ind_untreated_poorer_und"]
      #   } else {
      #     output_year[m,"ind_untreated_poorer_d"] <- output_year[m,"ind_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_untreated_poorest_d"] <- output_year[m,"ind_untreated_poorest_und"]
      #   } else {
      #     output_year[m,"ind_untreated_poorest_d"] <- output_year[m,"ind_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_untreated_richest_d"] <- output_year[m,"total_untreated_richest_und"]
      #   } else {
      #     output_year[m,"total_untreated_richest_d"] <- output_year[m,"total_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_untreated_richer_d"] <- output_year[m,"total_untreated_richer_und"]
      #   } else {
      #     output_year[m,"total_untreated_richer_d"] <- output_year[m,"total_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_untreated_middle_d"] <- output_year[m,"total_untreated_middle_und"]
      #   } else {
      #     output_year[m,"total_untreated_middle_d"] <- output_year[m,"total_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_untreated_poorer_d"] <- output_year[m,"total_untreated_poorer_und"]
      #   } else {
      #     output_year[m,"total_untreated_poorer_d"] <- output_year[m,"total_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_untreated_poorest_d"] <- output_year[m,"total_untreated_poorest_und"]
      #   } else {
      #     output_year[m,"total_untreated_poorest_d"] <- output_year[m,"total_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   
      #   ## Overall cases
      #   
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_richest_d"] <- output_year[m,"oop_richest_und"]
      #   } else {
      #     output_year[m,"oop_richest_d"] <- output_year[m,"oop_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_richer_d"] <- output_year[m,"oop_richer_und"]
      #   } else {
      #     output_year[m,"oop_richer_d"] <- output_year[m,"oop_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_middle_d"] <- output_year[m,"oop_middle_und"]
      #   } else {
      #     output_year[m,"oop_middle_d"] <- output_year[m,"oop_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_poorer_d"] <- output_year[m,"oop_poorer_und"]
      #   } else {
      #     output_year[m,"oop_poorer_d"] <- output_year[m,"oop_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"oop_poorest_d"] <- output_year[m,"oop_poorest_und"]
      #   } else {
      #     output_year[m,"oop_poorest_d"] <- output_year[m,"oop_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_richest_d"] <- output_year[m,"nm_richest_und"]
      #   } else {
      #     output_year[m,"nm_richest_d"] <- output_year[m,"nm_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_richer_d"] <- output_year[m,"nm_richer_und"]
      #   } else {
      #     output_year[m,"nm_richer_d"] <- output_year[m,"nm_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_middle_d"] <- output_year[m,"nm_middle_und"]
      #   } else {
      #     output_year[m,"nm_middle_d"] <- output_year[m,"nm_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_poorer_d"] <- output_year[m,"nm_poorer_und"]
      #   } else {
      #     output_year[m,"nm_poorer_d"] <- output_year[m,"nm_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"nm_poorest_d"] <- output_year[m,"nm_poorest_und"]
      #   } else {
      #     output_year[m,"nm_poorest_d"] <- output_year[m,"nm_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_richest_d"] <- output_year[m,"ind_richest_und"]
      #   } else {
      #     output_year[m,"ind_richest_d"] <- output_year[m,"ind_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_richer_d"] <- output_year[m,"ind_richer_und"]
      #   } else {
      #     output_year[m,"ind_richer_d"] <- output_year[m,"ind_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_middle_d"] <- output_year[m,"ind_middle_und"]
      #   } else {
      #     output_year[m,"ind_middle_d"] <- output_year[m,"ind_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_poorer_d"] <- output_year[m,"ind_poorer_und"]
      #   } else {
      #     output_year[m,"ind_poorer_d"] <- output_year[m,"ind_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"ind_poorest_d"] <- output_year[m,"ind_poorest_und"]
      #   } else {
      #     output_year[m,"ind_poorest_d"] <- output_year[m,"ind_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_richest_d"] <- output_year[m,"total_richest_und"]
      #   } else {
      #     output_year[m,"total_richest_d"] <- output_year[m,"total_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_richer_d"] <- output_year[m,"total_richer_und"]
      #   } else {
      #     output_year[m,"total_richer_d"] <- output_year[m,"total_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_middle_d"] <- output_year[m,"total_middle_und"]
      #   } else {
      #     output_year[m,"total_middle_d"] <- output_year[m,"total_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_poorer_d"] <- output_year[m,"total_poorer_und"]
      #   } else {
      #     output_year[m,"total_poorer_d"] <- output_year[m,"total_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      #   if (output_year$year[m]<disc_year) {
      #     output_year[m,"total_poorest_d"] <- output_year[m,"total_poorest_und"]
      #   } else {
      #     output_year[m,"total_poorest_d"] <- output_year[m,"total_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
      #   }
      # }
      
      ################################################################################
      ### PERCENTAGE OF HOUSEHOLDS EXPERIENCING CATASTROPHIC COSTS
      ################################################################################
      
      ## Adjust CC/OOP proportions from TB quintiles in PCS to population quintiles
      # % treated experiencing catastrophic costs
      cc1 <- c(cc_cty[counter,"cc_poorest"],cc_cty[counter,"cc_poorer"],cc_cty[counter,"cc_middle"],cc_cty[counter,"cc_richer"],cc_cty[counter,"cc_richest"])
      cc2 <- rep(NA,5)
      v5 <- rep(cc1,each=1e5/5)
      for(l in 1:5) cc2[l] <- mean(v5[id[l]:id[l+1]])
      
      # % treated experiencing catastrophic direct medical costs
      oop1 <- c(oop_cty[counter,"oop_poorest"],oop_cty[counter,"oop_poorer"],oop_cty[counter,"oop_middle"],oop_cty[counter,"oop_richer"],oop_cty[counter,"oop_richest"])
      oop2 <- rep(NA,5)
      v6 <- rep(oop1,each=1e5/5)
      for(l in 1:5) oop2[l] <- mean(v6[id[l]:id[l+1]])
      
      # % untreated experiencing catastrophic costs
      cc3 <- c(cc_cty_0.5[counter,"cc_poorest"],cc_cty_0.5[counter,"cc_poorer"],cc_cty_0.5[counter,"cc_middle"],cc_cty_0.5[counter,"cc_richer"],cc_cty_0.5[counter,"cc_richest"])
      cc4 <- rep(NA,5)
      v7 <- rep(cc3,each=1e5/5)
      for(l in 1:5) cc4[l] <- mean(v7[id[l]:id[l+1]])
      
      # % untreated experiencing catastrophic direct medical costs
      oop3 <- c(oop_cty_0.5[counter,"oop_poorest"],oop_cty_0.5[counter,"oop_poorer"],oop_cty_0.5[counter,"oop_middle"],oop_cty_0.5[counter,"oop_richer"],oop_cty_0.5[counter,"oop_richest"])
      oop4 <- rep(NA,5)
      v8 <- rep(oop3,each=1e5/5)
      for(l in 1:5) oop4[l] <- mean(v8[id[l]:id[l+1]])
      
      ## Assess catastrophic costs among treated cases
      cc_treated_richest <- cc2[5]*notif_richest
      cc_treated_richer <- cc2[4]*notif_richer
      cc_treated_middle <- cc2[3]*notif_middle
      cc_treated_poorer <- cc2[2]*notif_poorer
      cc_treated_poorest <- cc2[1]*notif_poorest
      
      oop_treated_richest <- oop2[5]*notif_richest
      oop_treated_richer <- oop2[4]*notif_richer
      oop_treated_middle <- oop2[3]*notif_middle
      oop_treated_poorer <- oop2[2]*notif_poorer
      oop_treated_poorest <- oop2[1]*notif_poorest
      
      ## Assess catastrophic costs among untreated cases
      cc_untreated_richest <- cc4[5]*untreated_richest
      cc_untreated_richer <- cc4[4]*untreated_richer
      cc_untreated_middle <- cc4[3]*untreated_middle
      cc_untreated_poorer <- cc4[2]*untreated_poorer
      cc_untreated_poorest <- cc4[1]*untreated_poorest
      
      oop_untreated_richest <- oop4[5]*untreated_richest
      oop_untreated_richer <- oop4[4]*untreated_richer
      oop_untreated_middle <- oop4[3]*untreated_middle
      oop_untreated_poorer <- oop4[2]*untreated_poorer
      oop_untreated_poorest <- oop4[1]*untreated_poorest
      
      ## Assess catastrophic costs among all cases
      output_year$cc_richest <- cc_treated_richest + cc_untreated_richest
      output_year$cc_richer <- cc_treated_richer + cc_untreated_richer
      output_year$cc_middle <- cc_treated_middle + cc_untreated_middle
      output_year$cc_poorer <- cc_treated_poorer + cc_untreated_poorer
      output_year$cc_poorest <- cc_treated_poorest + cc_untreated_poorest
      
      output_year$oop_richest <- oop_treated_richest + oop_untreated_richest
      output_year$oop_richer <- oop_treated_richer + oop_untreated_richer
      output_year$oop_middle <- oop_treated_middle + oop_untreated_middle
      output_year$oop_poorer <- oop_treated_poorer + oop_untreated_poorer
      output_year$oop_poorest <- oop_treated_poorest + oop_untreated_poorest
      
      ## Collate by country, UID, delivery scenario, coverage scenario
      output_names <- colnames(output_year)
      output <- as.data.frame(cbind(unique(base_cty$country),i,unique(base_cty$region),unique(base_cty$income),j,k,output_year))      
      colnames(output) <- c("country","code","region","income","UID","scenario",output_names)
   
      ## Append to overall output dataframe
      output_all <- rbind(output_all,output)
      
    }

  }

  ## Save output for each country
  write.csv(output_all,paste0("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/equity_0.5/",i,"_equity.csv"),row.names=FALSE)
  rm(output)
  
}
tbequity_0.5(index)
