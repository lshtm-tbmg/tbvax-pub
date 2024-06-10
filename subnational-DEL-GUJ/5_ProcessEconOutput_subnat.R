# This script takes model output, cost outputs, and DALY simulations, and 
# combines them for use in "econ_analysis"
rm(list=ls(all=TRUE))

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(arrow)
  library(magrittr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(qs)
  library(patchwork)
  library(readxl)  
  
  ## Set "country"/"state" name 
  i <- "DEL"
  
  ## Set paths to directories
  home <- "Econ/"
  epi_directory <- "Econ/TBVAX output/"
  
  setwd(here("./Econ/Data"))
  
})

# Load in inputs
wastage <- 0.05
r <- 0.03 # discount rate
start_year <- 2023 # first year of analytic results

## Load GBD aspirational life expectancy table
ledf <- read.csv("tmrlt_gbd2019_single_year_formatted-shape.csv", header=TRUE, check.names=FALSE)
ledf <- setDT(ledf)
ledf_l <- melt(ledf, id.vars = "Year", variable.name = "Age", value.name = "CLE") #Convert to long form
ledf_l$Age <- as.numeric(ledf_l$Age)
ledf_l <- subset(ledf_l,ledf_l$Age<100&ledf_l$Year>=start_year)

## Load country life expectancy table
lex <- read.csv("WPP2019_Life_Table_Medium_use.csv",header=TRUE,check.names=FALSE)

# Load disability weight parameter sets
dwTB    <- read.csv("dwTB_pred_sets.csv", header = TRUE, check.names = FALSE)
dwTBHIV <- read.csv("dwTBHIV_pred_sets.csv", header = TRUE, check.names = FALSE)
dwHIV   <- read.csv("dwHIV_pred_sets.csv", header = TRUE, check.names = FALSE)
dwART   <- read.csv("dwART_pred_sets.csv", header = TRUE, check.names = FALSE)


# Load in vaccine parameter sets
vax_supply    <- read.csv("vax_supply_pred_sets.csv", header = TRUE, check.names = FALSE)
vax_intro_ado <- read.csv("vax_intro_ado_pred_sets.csv", header = TRUE, check.names = FALSE)
vax_time_cost <- read.csv("vax_time_cost_pred_sets.csv", header = TRUE, check.names = FALSE)

if (i == "GUJ"){
  state_adj = 1
} else if (i == "DEL"){
  state_adj = 0.307773945
}


# Load in the data table with the GDP
base_cty <- read.csv("gdp-capita.csv", header=TRUE, check.names = FALSE)
base_cty <- base_cty[which(base_cty$Code == i),]

# Load cost files - which have the output of the cost parameter draws
setwd(here("Econ/Data"));setwd(i)

treat_ds_cty    <- read.csv("treat_ds_pred_sets.csv", header=TRUE, check.names=FALSE)
treat_rr_cty    <- read.csv("treat_rr_pred_sets.csv", header=TRUE, check.names=FALSE)
test_ds_cty     <- read.csv("test_ds_pred_sets.csv", header=TRUE, check.names=FALSE)
test_rr_cty     <- read.csv("test_rr_pred_sets.csv", header=TRUE, check.names=FALSE)
costnm_ds_cty   <- read.csv("costnm_ds_pred_sets.csv", header=TRUE, check.names=FALSE)
costind_ds_cty  <- read.csv("costind_ds_pred_sets.csv", header=TRUE, check.names=FALSE)
costnm_rr_cty   <- read.csv("costnm_rr_pred_sets.csv", header = TRUE, check.names=FALSE)
costind_rr_cty  <- read.csv("costind_rr_pred_sets.csv", header = TRUE, check.names=FALSE)
costdel_ado_cty <- read.csv("costdel_ado_pred_sets.csv", header = TRUE, check.names=FALSE)

# Start making the output dataframe (output_year)
output_year <- as.data.frame(seq(start_year, 2050, 1))
colnames(output_year) <- "Year"
output_all <- NULL
output_disaggregated_all <- NULL

## Grab list of UIDs from the file structure
setwd(here(home))
setwd(paste0("./TBVAX output/", i, "_TB"))
UID_list <- list.files()
UID_list <- tools::file_path_sans_ext(UID_list)


# Set counters to zero
uid_counter=0
counter <- 0

setwd(here(home))

# Outer of two loops which cycles through the UIDs, loads in epi data and costs
for (j0 in 1:length(UID_list)) {
  j <- UID_list[j0]
  
  ## Load data
  setwd(paste0(epi_directory, i, "_TB"))
  cty <- read_parquet(paste0(j,".parquet")) %>% collect()
  cty <- cty[Year <= 2050]
  cty <- cty[, Scenario := Runtype]
  
  setwd(paste0(epi_directory, i, "_alldeaths"))
  cty_deaths <- read_parquet(paste0(j,".parquet")) %>% collect()
  cty_deaths <- cty_deaths[Year <= 2050]
  cty_deaths <- cty_deaths[, Scenario := Runtype]
  
  setwd(paste0(epi_directory, i, "_TB_HIV"))
  cty_hiv <- read_parquet(paste0(j,".parquet")) %>% collect()
  cty_hiv <- cty_hiv[Year <= 2050]
  cty_hiv <- cty_hiv[, Scenario := Runtype]
  
  Scenario_u <-unique(cty$Scenario)
  vax_intro <- as.data.frame(str_extract_all(Scenario_u, "\\d{4}", simplify = T))
  
  # For ZAF only, get ART costs
  if(i=="ZAF"){
    art_cty <- art[which(art$code==i),]
  }
  
  # Imputation for  80-89 and 90-99 year olds,
  # Load lx proportions for 80-89 / 90-99 adjustment
  setwd(home);setwd("Data")
  lx_all <- read.csv("lx_single.csv",header=TRUE,check.names=FALSE)
  
  ## Subset lx proportions for 80-89 / 90-99 adjustment to country
  lx <- lx_all[which(lx_all$Code==i),]
  
  #~~
  ## Subset life expectancy table to country
  lex_cty <- as.data.frame(lex[which(lex$code==i&lex$AgeGrpStart<100),"ex"])
  colnames(lex_cty) <- "ex"   
  
  # The following backs out numeric values for the first age shown (Age) in AgeGrp, 
  # regexpr() gives the ctyex of given characters, 
  # and substr() extracts a piece of the character vector. 
  cty$Age <- as.numeric(substr(cty$AgeGrp,2,regexpr(",",cty$AgeGrp)-1))
  cty_hiv$Age <- as.numeric(substr(cty_hiv$AgeGrp,2,regexpr(",",cty_hiv$AgeGrp)-1))
  cty_deaths$Age <- as.numeric(substr(cty_deaths$AgeGrp,2,regexpr(",",cty_deaths$AgeGrp)-1))
  
  ## Interpolate 80-89, 90-99 age groups
  # Deaths data frame
  cty_deaths_80_89_temp <- cty_deaths[which(cty_deaths$Age==80),]
  cty_deaths_80_89      <- cty_deaths_80_89_temp[rep(seq_len(nrow(cty_deaths_80_89_temp)),each=10),]
  cty_deaths_80_89$Age  <- rep(seq(80,89,1),nrow(cty_deaths_80_89_temp))
  lx_80_89              <- as.vector(lx[which(lx$Age<90),"lx_per"])
  cty_deaths_80_89$lx   <- rep(lx_80_89,nrow(cty_deaths_80_89_temp))
  cty_deaths_80_89$ALLdeaths_adj <- cty_deaths_80_89$ALLdeaths * cty_deaths_80_89$lx
  cty_deaths_80_89_merge <- cty_deaths_80_89[,1:10]
  cty_deaths_80_89_merge$ALLdeaths <- cty_deaths_80_89$ALLdeaths_adj
  
  cty_deaths_90_99_temp <- cty_deaths[which(cty_deaths$Age==90),]
  cty_deaths_90_99 <- cty_deaths_90_99_temp[rep(seq_len(nrow(cty_deaths_90_99_temp)),each=10),]
  cty_deaths_90_99$Age <- rep(seq(90,99,1),nrow(cty_deaths_90_99_temp))
  lx_90_99 <- as.vector(lx[which(lx$Age>=90),"lx_per"])
  cty_deaths_90_99$lx <- rep(lx_90_99,nrow(cty_deaths_90_99_temp))
  cty_deaths_90_99$ALLdeaths_adj <- cty_deaths_90_99$ALLdeaths * cty_deaths_90_99$lx
  cty_deaths_90_99_merge <- cty_deaths_90_99[,1:10]
  cty_deaths_90_99_merge$ALLdeaths <- cty_deaths_90_99$ALLdeaths_adj
  
  # Merge deaths data frames
  cty_deaths_temp <- cty_deaths[which(cty_deaths$Age<80),]
  cty_deaths <- rbind(cty_deaths_temp,cty_deaths_80_89_merge,cty_deaths_90_99_merge)
  cty_deaths <- cty_deaths[order(cty_deaths$UID,cty_deaths$Runtype,cty_deaths$Scenario,cty_deaths$Age,cty_deaths$Year),]
  
  # HIV data frame
  cty_hiv_80_89_temp <- cty_hiv[which(cty_hiv$Age==80),]
  cty_hiv_80_89 <- cty_hiv_80_89_temp[rep(seq_len(nrow(cty_hiv_80_89_temp)),each=10),]
  cty_hiv_80_89$Age <- rep(seq(80,89,1),nrow(cty_hiv_80_89_temp))
  lx_80_89 <- as.vector(lx[which(lx$Age<90),"lx_per"])
  cty_hiv_80_89$lx <- rep(lx_80_89,nrow(cty_hiv_80_89_temp))
  cty_hiv_80_89$hiv_adj <- cty_hiv_80_89$Raw_Value * cty_hiv_80_89$lx
  cty_hiv_80_89_merge <- cty_hiv_80_89[,1:10]
  cty_hiv_80_89_merge$Raw_Value <- cty_hiv_80_89$hiv_adj
  
  cty_hiv_90_99_temp <- cty_hiv[which(cty_hiv$Age==90),]
  cty_hiv_90_99 <- cty_hiv_90_99_temp[rep(seq_len(nrow(cty_hiv_90_99_temp)),each=10),]
  cty_hiv_90_99$Age <- rep(seq(90,99,1),nrow(cty_hiv_90_99_temp))
  lx_90_99 <- as.vector(lx[which(lx$Age>=90),"lx_per"])
  cty_hiv_90_99$lx <- rep(lx_90_99,nrow(cty_hiv_90_99_temp))
  cty_hiv_90_99$hiv_adj <- cty_hiv_90_99$Raw_Value * cty_hiv_90_99$lx
  cty_hiv_90_99_merge <- cty_hiv_90_99[,1:10]
  cty_hiv_90_99_merge$Raw_Value <- cty_hiv_90_99$hiv_adj
  
  # Merge HIV data frames
  cty_hiv_temp <- cty_hiv[which(cty_hiv$Age<80),]
  cty_hiv <- rbind(cty_hiv_temp,cty_hiv_80_89_merge,cty_hiv_90_99_merge)
  cty_hiv <- cty_hiv[order(cty_hiv$UID,cty_hiv$Runtype,cty_hiv$Scenario,cty_hiv$HIV,cty_hiv$Age,cty_hiv$Year,cty_hiv$TB),]
  
  
  # We now tell the econ parts which model states refer to different states which map to TB DALY weights +- HIV
  # Define TB states for prevalent TB for DALY calculations
  # Dc is clinical TB, T is on-treatment, prevalent clinical TB is Dc+T
  hiv_list <- unique(cty_hiv$HIV)
  
  if (length(hiv_list) == 1) {
    tb_hiv_neg_sub <- cty_hiv[which((cty_hiv$TB=="Dc" | cty_hiv$TB=="T")
                                    & (cty_hiv$HIV=="HIV-" | cty_hiv$HIV == "NA")),]
  } else {
    tb_hiv_neg_sub <- cty_hiv[which((cty_hiv$TB=="Dc" | cty_hiv$TB=="T") & cty_hiv$HIV=="HIV-"),]
    tb_hiv_pos_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                    & (cty_hiv$HIV=="HIVu1"|cty_hiv$HIV=="HIVu2"|cty_hiv$HIV=="HIVd1"|cty_hiv$HIV=="HIVd2")),]
    tb_hiv_art_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                    & (cty_hiv$HIV=="ARTn1"|cty_hiv$HIV=="ARTn2"|cty_hiv$HIV=="ARTs1"|cty_hiv$HIV=="ARTs2")),]
    hiv_pos_sub <- cty_hiv[which((cty_hiv$TB=="Lf"|cty_hiv$TB=="Ls"|cty_hiv$TB=="Ds"|cty_hiv$TB=="Un"|cty_hiv$TB=="L0"|cty_hiv$TB=="R")
                                 & (cty_hiv$HIV=="HIVu1"|cty_hiv$HIV=="HIVu2"|cty_hiv$HIV=="HIVd1"|cty_hiv$HIV=="HIVd2")),]
    hiv_art_sub <- cty_hiv[which((cty_hiv$TB=="Lf"|cty_hiv$TB=="Ls"|cty_hiv$TB=="Ds"|cty_hiv$TB=="Un"|cty_hiv$TB=="L0"|cty_hiv$TB=="R")
                                 & (cty_hiv$HIV=="ARTn1"|cty_hiv$HIV=="ARTn2"|cty_hiv$HIV=="ARTs1"|cty_hiv$HIV=="ARTs2")),]
    art_sub <- cty_hiv[which(cty_hiv$HIV=="ARTn1"|cty_hiv$HIV=="ARTn2"|cty_hiv$HIV=="ARTs1"|cty_hiv$HIV=="ARTs2"),]
  }
  
  cty <- cty[order(cty$Runtype,cty$Scenario,cty$UID,cty$Year,cty$Age),]
  cty_deaths <- cty_deaths[order(cty_deaths$Runtype,cty_deaths$Scenario,cty_deaths$UID,cty_deaths$Year,cty_deaths$Age),]
  cty_hiv <- cty_hiv[order(cty_hiv$Runtype,cty_hiv$Scenario,cty_hiv$UID,cty_hiv$Year,cty_hiv$Age),]
  
  ## Aggregate TB states
  tb_hiv_neg_uid <- aggregate(tb_hiv_neg_sub$Raw_Value,by=list(tb_hiv_neg_sub$Year,tb_hiv_neg_sub$Age,tb_hiv_neg_sub$HIV,tb_hiv_neg_sub$Runtype,tb_hiv_neg_sub$Scenario),FUN=sum)
  colnames(tb_hiv_neg_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")
  
  if(exists('tb_hiv_pos_sub')) {tb_hiv_pos_uid <- aggregate(tb_hiv_pos_sub$Raw_Value,by=list(tb_hiv_pos_sub$Year,tb_hiv_pos_sub$Age,tb_hiv_pos_sub$HIV,tb_hiv_pos_sub$Runtype,tb_hiv_pos_sub$Scenario),FUN=sum)}
  if(exists('tb_hiv_pos_uid')) {colnames(tb_hiv_pos_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")}
  if(exists('tb_hiv_art_sub')) {tb_hiv_art_uid <- aggregate(tb_hiv_art_sub$Raw_Value,by=list(tb_hiv_art_sub$Year,tb_hiv_art_sub$Age,tb_hiv_art_sub$HIV,tb_hiv_art_sub$Runtype,tb_hiv_art_sub$Scenario),FUN=sum)}
  if(exists('tb_hiv_art_uid')) {colnames(tb_hiv_art_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")}
  if(exists('hiv_pos_sub')) {hiv_pos_uid <- aggregate(hiv_pos_sub$Raw_Value,by=list(hiv_pos_sub$Year,hiv_pos_sub$Age,hiv_pos_sub$HIV,hiv_pos_sub$Runtype,hiv_pos_sub$Scenario),FUN=sum)}
  if(exists('hiv_pos_uid')) {colnames(hiv_pos_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")}
  if(exists('hiv_art_sub')) {hiv_art_uid <- aggregate(hiv_art_sub$Raw_Value,by=list(hiv_art_sub$Year,hiv_art_sub$Age,hiv_art_sub$HIV,hiv_art_sub$Runtype,hiv_art_sub$Scenario),FUN=sum)}
  if(exists('hiv_art_uid')) {colnames(hiv_art_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")}
  if(exists('art_sub')) {art_uid <- aggregate(art_sub$Raw_Value,by=list(art_sub$Year,art_sub$Age,art_sub$HIV,art_sub$Runtype,art_sub$Scenario),FUN=sum)}
  if(exists('art_uid')) {colnames(art_uid) <- c("Year","Age","HIV","Runtype","Scenario","Raw_Value")}
  
  ### Country life expectancy to single year ages
  le_under1 <- as.data.frame(lex_cty[1,])
  le_1to4   <- rbind(lex_cty[2,],lex_cty[2,], lex_cty[2,], lex_cty[2,])
  le_5to99  <- as.data.frame(lex_cty[rep(seq_len(nrow(lex_cty)), each=5),])
  le_5to99  <- as.data.frame(le_5to99[11:nrow(le_5to99),])
  colnames(le_under1) <- "ex"
  colnames(le_1to4)   <- "ex"
  colnames(le_5to99)  <- "ex"
  age_le <- as.data.frame(seq(0,99,1))
  colnames(age_le) <- "Age"
  le_single <- rbind(le_under1,le_1to4,le_5to99)
  le_single <- cbind(age_le,le_single)
  
  # Increase econ parameter set counter
  counter <- counter + 1
  
  # Grab list of vaccine/coverage delivery scenarios which is in the Runtype column
  scenarios <- unique(cty$Runtype)
  
  # Interior k loop: delivery scenario
  
  for (k0 in 1:length(scenarios)) {
    k = scenarios[k0]
    
    # The following calculates DALYs by taking the relevant case and death data and applying DALY weights 
    inc_scen <- cty[which(cty$Runtype==k),]
    deaths_scen <- cty_deaths[which(cty_deaths$Runtype==k),]
    tb_hiv_neg_scen <- tb_hiv_neg_uid[which(tb_hiv_neg_uid$Runtype==k),]
    
    if(exists('tb_hiv_pos_uid')) {tb_hiv_pos_scen <- tb_hiv_pos_uid[which(tb_hiv_pos_uid$Runtype==k),]}
    if(exists('tb_hiv_art_uid')) {tb_hiv_art_scen <- tb_hiv_art_uid[which(tb_hiv_art_uid$Runtype==k),]}
    if(exists('hiv_pos_uid')) {hiv_pos_scen <- hiv_pos_uid[which(hiv_pos_uid$Runtype==k),]}
    if(exists('hiv_art_uid')) {hiv_art_scen <- hiv_art_uid[which(hiv_art_uid$Runtype==k),]}
    if(exists('art_uid')) {art_scen <- art_uid[which(art_uid$Runtype==k),]}
    
    inc_cov <- inc_scen[which(inc_scen$Year>=start_year),]
    deaths_cov <- deaths_scen[which(deaths_scen$Year>=start_year),] 
    tb_hiv_neg_cov <- tb_hiv_neg_scen[which(tb_hiv_neg_scen$Year>=start_year),]
    
    if(exists('tb_hiv_pos_scen')) {tb_hiv_pos_cov <- tb_hiv_pos_scen[which(tb_hiv_pos_scen$Year>=start_year),]}
    if(exists('tb_hiv_art_scen')) {tb_hiv_art_cov <- tb_hiv_art_scen[which(tb_hiv_art_scen$Year>=start_year),]}
    if(exists('hiv_pos_scen')) {hiv_pos_cov <- hiv_pos_scen[which(hiv_pos_scen$Year>=start_year),]}
    if(exists('hiv_art_scen')) {hiv_art_cov <- hiv_art_scen[which(hiv_art_scen$Year>=start_year),]}
    if(exists('art_scen')) {art_cov <- art_scen[which(art_scen$Year>=start_year),]}
    
    # Merge YLL working data frame
    YLL_working <- merge(ledf_l, deaths_cov, by=c("Year","Age"))
    YLL_working <- YLL_working[order(YLL_working$Year, YLL_working$Age),]
    
    # Raw YLLs
    YLL_working$YLL_raw <- YLL_working$ALLdeaths*YLL_working$CLE
    
    # Discount YLLs to time of death using continuous time discounting
    YLL_working$YLL_dtod <- YLL_working$ALLdeaths*((1 - exp(-r * YLL_working$CLE)) / r)
    
    # Sum YLLs across ages
    YLL_year <- aggregate(YLL_working$YLL_raw~YLL_working$Year,FUN=sum)[,2]
    YLL_year_d <- aggregate(YLL_working$YLL_dtod~YLL_working$Year,FUN=sum)[,2]
    total_deaths <- aggregate(YLL_working$ALLdeaths~YLL_working$Year,FUN=sum)[,2]
    
    # Sum productive YLLs
    YLL_prod <- YLL_working[which(YLL_working$Age>=15&YLL_working$Age<=49),]
    YLL_prod_year <- aggregate(YLL_prod$YLL_raw~YLL_prod$Year,FUN=sum)[,2]
    
    output_year$YLL_und <- YLL_year
    output_year$YLL_year_d <- YLL_year_d
    output_year$total_deaths <- total_deaths
    
    # Merge country life expectancy working data frame
    LEX_working <- merge(le_single,deaths_cov,by="Age")
    LEX_working <- LEX_working[order(LEX_working$Year,LEX_working$Age),]
    
    # Raw LEX
    LEX_working$LEX_raw <- LEX_working$ALLdeaths*LEX_working$ex
    
    # Discount LEX to time of death using continuous time discounting
    LEX_working$LEX_dtod <- LEX_working$ALLdeaths*((1 - exp(-r * LEX_working$ex)) / r)
    
    # Sum LEX across ages
    LEX_year <- aggregate(LEX_working$LEX_raw~LEX_working$Year,FUN=sum)[,2]
    LEX_year_d <- aggregate(LEX_working$LEX_dtod~LEX_working$Year,FUN=sum)[,2]
    output_year$LEX_und <- LEX_year
    output_year$LEX_year_d <- LEX_year_d
    
    # Aggregate YLDs by year from HIV dataframe
    tb_hiv_neg_cov_sub <- tb_hiv_neg_cov[which(tb_hiv_neg_cov$Year>=start_year),]
    if (length(hiv_list) == 1) {
      YLD_year_temp <- aggregate(tb_hiv_neg_cov_sub$Raw_Value~tb_hiv_neg_cov_sub$Year,FUN=sum)[,2]
      YLD_year <- YLD_year_temp * dwTB[counter,1]
    } else if (length(hiv_list)>1){
      if(exists('tb_hiv_pos_cov')) {YLD_year_tb_hiv_pos <- aggregate(tb_hiv_pos_cov$Raw_Value~tb_hiv_pos_cov$Year, FUN=sum)[,2]}
      if(exists('tb_hiv_art_cov')) {YLD_year_tb_hiv_art <- aggregate(tb_hiv_art_cov$Raw_Value~tb_hiv_art_cov$Year, FUN=sum)[,2]}
      if(exists('hiv_pos_cov')) {YLD_year_hiv_pos <- aggregate(hiv_pos_cov$Raw_Value~hiv_pos_cov$Year, FUN=sum)[,2]}
      if(exists('hiv_art_cov')) {YLD_year_hiv_art <- aggregate(hiv_art_cov$Raw_Value~hiv_art_cov$Year, FUN=sum)[,2]}
      if(exists('art_cov')) {YLD_year_art <- aggregate(art_cov$Raw_Value~art_cov$Year, FUN=sum)[,2]}
      YLD_year_temp <- aggregate(tb_hiv_neg_cov_sub$Raw_Value~tb_hiv_neg_cov_sub$Year,FUN=sum)[,2]
      YLD_year <- YLD_year_temp * dwTB[counter,1] + YLD_year_tb_hiv_pos * dwTBHIV[counter,1] +
        YLD_year_tb_hiv_art * dwTBHIV[counter,1] + YLD_year_hiv_pos * dwHIV[counter,1] +
        YLD_year_hiv_art * dwART[counter,1]
    }
    output_year$YLD_und <- YLD_year
    
    # Here we discount the DALYs, and do some more adding up of important things like cases and deaths
    for (m in 1:nrow(output_year)) {
      if (output_year$Year[m]<start_year) {
        output_year[m,"YLL_d"] <- output_year[m,"YLL_year_d"]
      } else {
        output_year[m,"YLL_d"] <- output_year[m,"YLL_year_d"] / (1+r)^(output_year$Year[m]-start_year)
      }
      if (output_year$Year[m]<start_year) {
        output_year[m,"YLD_d"] <- output_year[m,"YLD_und"]
      } else {
        output_year[m,"YLD_d"] <- output_year[m,"YLD_und"] / (1+r)^(output_year$Year[m]-start_year)
      }
    }
    
    # Sum YLLs + YLDs = DALYs
    output_year$dalys_und <- output_year$YLL_und + output_year$YLD_und
    output_year$dalys_d <- output_year$YLL_d + output_year$YLD_d
    
    ## Aggregate TB cases (notifications)
    treated_year <- aggregate(inc_cov$Total_Notif~inc_cov$Year,FUN=sum)[,2]
    
    ## Aggregate treated cases by scenario, DS/RR
    notif_DS_year <- aggregate(inc_cov$Total_Notif_DS~inc_cov$Year,FUN=sum)[,2]
    notif_RR_year <- aggregate(inc_cov$Total_Notif_RR~inc_cov$Year,FUN=sum)[,2]
    
    ## Aggregate ART treatments by scenario
    if(exists('art_cov')) {hiv_art_year <- aggregate(art_cov$Raw_Value~art_cov$Year,FUN=sum)[,2]}
    
    ## Aggregated vaccinated individuals by scenario
    if (k=="baseline") {
      vax_year <- rep(0,nrow(output_year))
    } else {
      vax_year <- aggregate(inc_cov$Number_VXa~inc_cov$Year,FUN=sum)[,2]
    }
    
    vax_year <- as.data.frame(vax_year)
    
    ## Estimate vaccine introduction year timeline by scenario
    year_1 = 0
    if (k=="baseline") {
      vax_year[,2] <- 0
      vax_year[,3] <- 0
    } else {
      for (m in 1:nrow(vax_year)) {
        if (vax_year[m,1]==0) {
          vax_year[m,2] <- 0
        }
        else {
          vax_year[m,2] <- vax_year[m-1,2] + 1
          if (vax_year[m,2] == 1 & year_1 == 0){
            year_1 = year_1 + 1
          } else if (vax_year[m,2] == 1 & year_1 != 0) {
            year_1 = year_1 + 1
            vax_year[m,2] <-max(vax_year[1:m-1,2]) + 1
          }
        }
      }
    }
    
    for (m in 1:nrow(vax_year)){
      if (vax_year[m,2] != 1 & vax_year[m,2]%%10 == 1 & m != max(nrow(vax_year))){
        vax_year[m,3] <- 1
      } else {
        vax_year[m,3] <- 0
      } 
    }
    
    if (k == "M72_AI_POD_50_10yr_med_2030_diffages"){
      vax_year[vax_year[,2]==6,3] <- 1
    }
    
    
    #'The following section multiplies different unit costs with
    #' numbers of people moving through the different states
    
    ## Calculate testing and treatment costs by scenario, DS/RR
    output_year$test_ds_und  <- notif_DS_year*test_ds_cty[,counter+4]
    output_year$test_rr_und  <- notif_RR_year*test_rr_cty[,counter+4]
    output_year$treat_ds_und <- notif_DS_year*treat_ds_cty[,counter+4]
    output_year$treat_rr_und <- notif_RR_year*treat_rr_cty[,counter+4]
    
    
    ## Calculate ART costs
    if(exists('hiv_art_year')) {
      art_und <- hiv_art_year*art_cty[,counter+4]
      output_year$art_und <- art_und
    }
    
    ## Calculate non-medical costs by scenario, DS/RR
    output_year$costnm_ds_und <- notif_DS_year*costnm_ds_cty[,counter+4]
    output_year$costnm_rr_und <- notif_RR_year*costnm_rr_cty[,counter+4]
    
    ## Calculate indirect costs, including productivity loss due to premature death
    output_year$ind_ds_und <- notif_DS_year*costind_ds_cty[,counter+4]
    output_year$ind_rr_und <- notif_RR_year*costind_ds_cty[,counter+4]
    
    # Calculate productivity costs
    output_year$prod_cost_und <- output_year$LEX_year_d*base_cty$GDPpc
    
    ## Calculate vaccine costs by scenario, including introduction costs in year 1
    if (grepl("M72", k)==TRUE) {
      if (grepl("diffages", k)==TRUE){
        intro_pop <- inc_cov[which(inc_cov$Age==17),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=18 & inc_cov$Age<=55),]
      } else if (grepl("alladults",k)==TRUE){
        intro_pop <- inc_cov[which(inc_cov$Age==18),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=19),]
      } else {
        intro_pop <- inc_cov[which(inc_cov$Age==15),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=16 & inc_cov$Age<=34),]
      }
    } 
    
    if (grepl("BCG", k)==TRUE){
      if (grepl("diffages", k)==TRUE){
        intro_pop <- inc_cov[which(inc_cov$Age==15),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=16 & inc_cov$Age<=34),]
      } else if (grepl("alladults",k)==TRUE){
        intro_pop <- inc_cov[which(inc_cov$Age==18),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=19),]
      } else {
        intro_pop <- inc_cov[which(inc_cov$Age==10),]
        campaign_sub <- inc_cov[which(inc_cov$Age>=11 & inc_cov$Age<=18),]
      }
    } 
    
    if (k!="baseline") {
      campaign_pop <- aggregate(campaign_sub$Population~campaign_sub$Year,FUN=sum)
      colnames(campaign_pop) <- c("Year","Population")    
    }
    
    
    #' Different cost scenarios, based on the scenario (k)
    
    if (k=="baseline") {
      vax_price <- 0
    } 
    if (grepl("M72",k)==TRUE) {
      vax_price <- 2.5
      number_doses <- 2
    } 
    
    if (grepl("BCG",k) == TRUE){
      vax_price <- 0.17
      number_doses <- 1
    }
    
    scaleup_yrs <- c(1:5)
    
    ###########################
    # Made changes with the vax_intro and vax_supply and added vax_cost_soc wth vax time cost
    ###########################
    if (k=="baseline") {
      output_year$vax_cost_und <- 0
      output_year$vax_cost_soc_und <- 0
    } else {
      for (m in 1:nrow(vax_year)) {
        if (vax_year[m,2]==0) {
          output_year$vax_cost_und[m] <- 0 
        }
        if (vax_year[m,2]==1 & vax_year[m,3] == 0) {
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
        if (vax_year[m,2]==2 & vax_year[m,3] == 0) {
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
        if (vax_year[m,2]==3 & vax_year[m,3] == 0) {
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
        if (vax_year[m,2]==4 & vax_year[m,3] == 0) {
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
        if (vax_year[m,2]==5 & vax_year[m,3] == 0) {
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4])*0.2 + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
        if (!(vax_year[m,2] %in% scaleup_yrs) & vax_year[m,3]==0) {# if it's not in scale-up years and not a repeat 
          output_year$vax_cost_und[m] <- (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        } 
        if (vax_year[m,3]==1){ # if we have a repeat, add intro costs again
          output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4]) + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          output_year$vax_cost_soc_und[m] <- (intro_pop$Population[m]*vax_intro_ado[,counter+4]) + (vax_year[m,1]*(number_doses)*(vax_price + vax_supply[,counter+4] + as.numeric(costdel_ado_cty[,counter+4]) + vax_time_cost[,counter+4]*state_adj))*(1+wastage)
        }
      }
    }
    
    
    #'## Estimate discounted and undiscounted costs for all cost categories
    for (m in 1:nrow(output_year)) {
      
      if(exists('art_und')) {
        if (output_year$Year[m]<start_year) {
          output_year[m,"art_d"] <- output_year[m,"art_und"]
        } else {
          output_year[m,"art_d"] <- output_year[m,"art_und"] / (1+r)^(output_year$Year[m]-start_year)
        } 
      }
      
      if (output_year$Year[m]<start_year) {
        output_year[m,"test_ds_d"]   <- output_year[m,"test_ds_und"]
        output_year[m,"test_rr_d"]   <- output_year[m,"test_rr_und"]
        output_year[m,"treat_ds_d"]  <- output_year[m,"treat_ds_und"]
        output_year[m,"treat_rr_d"]  <- output_year[m,"treat_rr_und"]
        output_year[m,"vax_cost_d"]  <- output_year[m,"vax_cost_und"]
        output_year[m,"costnm_ds_d"] <- output_year[m,"costnm_ds_und"]
        output_year[m,"costnm_rr_d"] <- output_year[m,"costnm_rr_und"]
        output_year[m,"ind_ds_d"] <- output_year[m,"ind_ds_und"]
        output_year[m,"ind_rr_d"] <- output_year[m,"ind_rr_und"]
        output_year[m,"prod_cost_d"] <- output_year[m,"prod_cost_und"]
        output_year[m,"vax_cost_soc_d"] <- output_year[m,"vax_cost_soc_und"]
      } else {
        output_year[m,"test_ds_d"]   <- output_year[m,"test_ds_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"test_rr_d"]   <- output_year[m,"test_rr_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"treat_ds_d"]  <- output_year[m,"treat_ds_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"treat_rr_d"]  <- output_year[m,"treat_rr_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"vax_cost_d"]  <- output_year[m,"vax_cost_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"costnm_ds_d"] <- output_year[m,"costnm_ds_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"costnm_rr_d"] <- output_year[m,"costnm_rr_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"ind_ds_d"] <- output_year[m,"ind_ds_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"ind_rr_d"] <- output_year[m,"ind_rr_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"prod_cost_d"] <- output_year[m,"prod_cost_und"] / (1+r)^(output_year$Year[m]-start_year)
        output_year[m,"vax_cost_soc_d"] <- output_year[m,"vax_cost_soc_und"] / (1+r)^(output_year$Year[m]-start_year)
      }
    }
    
    
    ## Collate DALYs and each cost category by country, year, UID, delivery scenario for undiscounted/discounted
    
    output_year <- setDT(output_year)
    
    if(exists('art_und')) {# HIV countries
      output_sum <- output_year[, .(country = i, code = i, year = Year, UID = j, scenario = k, total_deaths, dalys_und, dalys_d,
                                    disease_cost_und     = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + art_und,
                                    disease_cost_soc_und = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + art_und + costnm_ds_und + costnm_rr_und,
                                    cost_und     = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + art_und + vax_cost_und,
                                    cost_soc_und = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + art_und + vax_cost_soc_und + costnm_ds_und + costnm_rr_und,
                                    disease_cost_d     = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + art_d,
                                    disease_cost_soc_d = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + art_d + costnm_ds_d + costnm_rr_d,
                                    cost_d     = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + art_d + vax_cost_d,
                                    cost_soc_d = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + art_d + vax_cost_soc_d + costnm_ds_d + costnm_rr_d,
                                    test_ds_und, test_rr_und, treat_ds_und, treat_rr_und, art_und, vax_cost_und, vax_cost_soc_und,
                                    test_ds_d, test_rr_d, treat_ds_d, treat_rr_d, art_d, vax_cost_d, vax_cost_soc_d, costnm_ds_d, costnm_rr_d,
                                    disease_d_noart = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d,
                                    treat_sum_d = treat_ds_d + treat_rr_d,
                                    test_sum_d = test_ds_d + test_rr_d)]
    } else {# Non-HIV countries
      output_sum <- output_year[, .(country = i, code = i, year = Year, UID = j, scenario = k, dalys_und, dalys_d, total_deaths,
                                    disease_cost_und     = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und,
                                    disease_cost_soc_und = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + costnm_ds_und + costnm_rr_und,
                                    cost_und     = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + vax_cost_und,
                                    cost_soc_und = test_ds_und + test_rr_und + treat_ds_und + treat_rr_und + vax_cost_soc_und + costnm_ds_und + costnm_rr_und + ind_ds_und + ind_rr_und,
                                    disease_cost_d     = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d,
                                    disease_cost_soc_d = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + costnm_ds_d + costnm_rr_d,
                                    cost_d     = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + vax_cost_d,
                                    cost_soc_d = test_ds_d + test_rr_d + treat_ds_d + treat_rr_d + vax_cost_soc_d + costnm_ds_d + costnm_rr_d + ind_ds_d + ind_rr_d,
                                    test_ds_und, test_rr_und, treat_ds_und, treat_rr_und, vax_cost_und, vax_cost_soc_und,
                                    test_ds_d, test_rr_d, treat_ds_d, treat_rr_d, vax_cost_d, vax_cost_soc_d, 
                                    costnm_ds_d, costnm_rr_d, ind_ds_d, ind_rr_d, 
                                    costnm_ds_und, costnm_rr_und, ind_ds_und, ind_rr_und, 
                                    treat_sum_d = treat_ds_d + treat_rr_d,
                                    test_sum_d  = test_ds_d + test_rr_d)]
    }
    
    ## Append to overall output dataframe
    output_all <- rbind(output_all, output_sum)
    
  }
  
  uid_counter <- uid_counter+1
  print(uid_counter) 
  
}


#Export the overall long form output
setwd(here("Econ/TBVAX econ output"))
write.csv(output_all, paste0(i, "_hs_cea.csv"), row.names = FALSE)


# --- end

