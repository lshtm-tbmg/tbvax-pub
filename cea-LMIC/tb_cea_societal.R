## Load packages
library(dplyr)
library(tidyr)
library(data.table)
library(arrow)
library(magrittr)
library(stringr)
library(reshape2)

## Create lists of countries
setwd("CEA") # CHANGE: Set working directory with input files ("CEA folder")
countries <- read.csv("LMIC_emulation_revised.csv",header=TRUE,check.names=FALSE)[,2]

## Set vaccine inputs
vax_price <- 4.6
vax_supply <- 0.11
vax_intro_ado <- 2.4
vax_intro_inf <- 0.65
wastage <- 0.05

## Specify discount rate & analytic years
r <- 0.03 #discount rate
start_year <- 2024 #first year of analytic results
end_year <- 2050 #end year of analytic results 

### Load DALY inputs

## Load GBD aspirational life expectancy table
ledf <- read.csv("tmrlt_gbd2019_single_year_formatted-shape.csv",header=TRUE,check.names=FALSE) #Life expectancy data frame
ledf_l <- melt(ledf, id.vars = "Year", variable.name = "Age", value.name = "CLE") #Convert to long form
ledf_l$Age <- as.numeric(ledf_l$Age)
ledf_l <- subset(ledf_l,ledf_l$Age<100&ledf_l$Year>=start_year)

## Load country life expectancy table
lex <- read.csv("WPP2019_Life_Table_Medium_use.csv",header=TRUE,check.names=FALSE) #Life expectancy data frame

## Load disability weight parameter sets
dwTB <- read.csv("dwTB_pred_sets.csv",header=TRUE,check.names=FALSE)
dwTBHIV <- read.csv("dwTBHIV_pred_sets.csv",header=TRUE,check.names=FALSE)
dwHIV <- read.csv("dwHIV_pred_sets.csv",header=TRUE,check.names=FALSE)
dwART <- read.csv("dwART_pred_sets.csv",header=TRUE,check.names=FALSE)

## Load cost uncertainty sets

treat_ds <- read.csv("treat_ds_pred_sets.csv",header=TRUE,check.names=FALSE)
treat_rr <- read.csv("treat_rr_pred_sets.csv",header=TRUE,check.names=FALSE)
test_ds <- read.csv("test_ds_pred_sets.csv",header=TRUE,check.names=FALSE)
test_rr <- read.csv("test_rr_pred_sets.csv",header=TRUE,check.names=FALSE)
art <- read.csv("art_pred_sets.csv",header=TRUE,check.names=FALSE)
nm_ds <- read.csv("nm_ds_pred_sets.csv",header=TRUE,check.names=FALSE)
ind_ds <- read.csv("ind_ds_pred_sets.csv",header=TRUE,check.names=FALSE)
nm_rr <- read.csv("nm_rr_pred_sets.csv",header=TRUE,check.names=FALSE)
ind_rr <- read.csv("ind_rr_pred_sets.csv",header=TRUE,check.names=FALSE)
costdel_ado <- read.csv("costdel_ado_pred_sets.csv",header=TRUE,check.names=FALSE)
costdel_inf <- read.csv("costdel_inf_pred_sets.csv",header=TRUE,check.names=FALSE)
base <- read.csv("LMIC_emulation.csv",header=TRUE,check.names=FALSE)

# outermost loop = country

for (i0 in 1:length(countries)) {
  
  i <- countries[i0]

  # Initialize dataframe
  
  output_year <- as.data.frame(seq(start_year,end_year,1)) #~# EDITED 20-Sep-2022
  colnames(output_year) <- "Year"
  output_all <- NULL
  output_disaggregated_all <- NULL
  
  ## Grab list of UIDs
  
  directory <- paste0("") # CHANGE: Specify directory with tbvax output
  
  UID_list <- list.files(path = paste0(directory,"cc_TB/",i,"_TB/"))
  UID_list <- tools::file_path_sans_ext(UID_list)
  
  # Set econ parameter set counter to zero
  
  counter <- 0
  
  # second loop: uid
  
  for (j0 in 1:length(UID_list)) {
    
    j <- UID_list[j0]
    
    ## Load data
    
    cty <- read_parquet(paste0(directory,"cc_TB/",i,"_TB/",j,".parquet")) %>% collect()
    
    cty_deaths <- read_parquet(paste0(directory,"cc_alldeaths/",i,"_alldeaths/",j,".parquet")) %>% collect()
    
    cty_hiv <- read_parquet(paste0(directory,"cc_TB_HIV/",i,"_TB_HIV/",j,".parquet")) %>% collect()
    
    #~~
    ## Subset costs to country
    treat_ds_cty <- treat_ds[which(treat_ds$code==i),]
    treat_rr_cty <- treat_rr[which(treat_rr$code==i),]
    test_ds_cty <- test_ds[which(test_ds$code==i),]
    test_rr_cty <- test_rr[which(test_rr$code==i),]
    art_cty <- art[which(art$code==i),]
    nm_ds_cty <- nm_ds[which(nm_ds$code==i),]
    ind_ds_cty <- ind_ds[which(ind_ds$code==i),]
    nm_rr_cty <- nm_rr[which(nm_rr$code==i),]
    ind_rr_cty <- ind_rr[which(ind_rr$code==i),]
    costdel_ado_cty <- costdel_ado[which(costdel_ado$code==i),]
    costdel_inf_cty <- costdel_inf[which(costdel_inf$code==i),]
    base_cty <- base[which(base$code==i),]
    
    ## Load lx proportions for 80-89 / 90-99 adjustment
    
    lx_all <- read.csv("lx_single.csv",header=TRUE,check.names=FALSE)
    
    ## Subset lx proportions for 80-89 / 90-99 adjustment to country
    lx <- lx_all[which(lx_all$code==i),]
    
    ## Subset life expectancy table to country
    lex_cty <- as.data.frame(lex[which(lex$code==i&lex$AgeGrpStart<100),"ex"])
    colnames(lex_cty) <- "ex"
    
    # The following backs out numeric values for the first age shown (Age) in AgeGrp,
    # regexpr() gives the ctyex of given characters,
    # and substr() extracts a piece of the character vector.
    
    cty$Age <- as.numeric(substr(cty$AgeGrp,2,regexpr(",",cty$AgeGrp)-1))
    cty_hiv$Age <- as.numeric(substr(cty_hiv$AgeGrp,2,regexpr(",",cty_hiv$AgeGrp)-1))
    cty_deaths$Age <- as.numeric(substr(cty_deaths$AgeGrp,2,regexpr(",",cty_deaths$AgeGrp)-1))
    
    ## Multiply 80-89 and 90-99 age groups by 10 to address full number in group captured
    for (n in 1:nrow(cty)) {
      
      if (cty$Age[n]==80) {
        cty$Total_Notif[n] <- cty$Total_Notif[n]*10
        cty$Total_Notif_DS[n] <- cty$Total_Notif_DS[n]*10
        cty$Total_Notif_RR[n] <- cty$Total_Notif_RR[n]*10
        cty$Number_VXa[n] <- cty$Number_VXa[n]*10
      }
      if (cty$Age[n]==90) {
        cty$Total_Notif[n] <- cty$Total_Notif[n]*10
        cty$Total_Notif_DS[n] <- cty$Total_Notif_DS[n]*10
        cty$Total_Notif_RR[n] <- cty$Total_Notif_RR[n]*10
        cty$Number_VXa[n] <- cty$Number_VXa[n]*10
      }
      
    }
    
    ## Interpolate 80-89, 90-99 age groups
    # Deaths data frame
    cty_deaths_80_89_temp <- cty_deaths[which(cty_deaths$Age==80),]
    cty_deaths_80_89 <- cty_deaths_80_89_temp[rep(seq_len(nrow(cty_deaths_80_89_temp)),each=10),]
    cty_deaths_80_89$Age <- rep(seq(80,89,1),nrow(cty_deaths_80_89_temp))
    lx_80_89 <- as.vector(lx[which(lx$age<90),"lx_per"])
    cty_deaths_80_89$lx <- rep(lx_80_89,nrow(cty_deaths_80_89_temp))
    cty_deaths_80_89$ALLdeaths_adj <- cty_deaths_80_89$ALLdeaths * cty_deaths_80_89$lx * 10 #~# EDITED 21-Sep-2022
    cty_deaths_80_89_merge <- cty_deaths_80_89[,1:11]
    cty_deaths_80_89_merge$ALLdeaths <- cty_deaths_80_89$ALLdeaths_adj
    
    cty_deaths_90_99_temp <- cty_deaths[which(cty_deaths$Age==90),]
    cty_deaths_90_99 <- cty_deaths_90_99_temp[rep(seq_len(nrow(cty_deaths_90_99_temp)),each=10),]
    cty_deaths_90_99$Age <- rep(seq(90,99,1),nrow(cty_deaths_90_99_temp))
    lx_90_99 <- as.vector(lx[which(lx$age>=90),"lx_per"])
    cty_deaths_90_99$lx <- rep(lx_90_99,nrow(cty_deaths_90_99_temp))
    cty_deaths_90_99$ALLdeaths_adj <- cty_deaths_90_99$ALLdeaths * cty_deaths_90_99$lx * 10 #~# EDITED 21-Sep-2022
    cty_deaths_90_99_merge <- cty_deaths_90_99[,1:11]
    cty_deaths_90_99_merge$ALLdeaths <- cty_deaths_90_99$ALLdeaths_adj
    
    # Merge deaths data frames
    cty_deaths_temp <- cty_deaths[which(cty_deaths$Age<80),]
    cty_deaths <- rbind(cty_deaths_temp,cty_deaths_80_89_merge,cty_deaths_90_99_merge)
    cty_deaths <- cty_deaths[order(cty_deaths$UID,cty_deaths$Runtype,cty_deaths$Scenario,cty_deaths$Age,cty_deaths$Year),]
    cty_deaths[is.na(cty_deaths)] <- 0
    
    # HIV data frame
    cty_hiv_80_89_temp <- cty_hiv[which(cty_hiv$Age==80),]
    cty_hiv_80_89 <- cty_hiv_80_89_temp[rep(seq_len(nrow(cty_hiv_80_89_temp)),each=10),]
    cty_hiv_80_89$Age <- rep(seq(80,89,1),nrow(cty_hiv_80_89_temp))
    lx_80_89 <- as.vector(lx[which(lx$age<90),"lx_per"])
    cty_hiv_80_89$lx <- rep(lx_80_89,nrow(cty_hiv_80_89_temp))
    cty_hiv_80_89$hiv_adj <- cty_hiv_80_89$Raw_Value * cty_hiv_80_89$lx * 10 #~# EDITED 21-Sep-2022
    cty_hiv_80_89_merge <- cty_hiv_80_89[,1:11]
    cty_hiv_80_89_merge$Raw_Value <- cty_hiv_80_89$hiv_adj
    
    cty_hiv_90_99_temp <- cty_hiv[which(cty_hiv$Age==90),]
    cty_hiv_90_99 <- cty_hiv_90_99_temp[rep(seq_len(nrow(cty_hiv_90_99_temp)),each=10),]
    cty_hiv_90_99$Age <- rep(seq(90,99,1),nrow(cty_hiv_90_99_temp))
    lx_90_99 <- as.vector(lx[which(lx$age>=90),"lx_per"])
    cty_hiv_90_99$lx <- rep(lx_90_99,nrow(cty_hiv_90_99_temp))
    cty_hiv_90_99$hiv_adj <- cty_hiv_90_99$Raw_Value * cty_hiv_90_99$lx * 10 #~# EDITED 21-Sep-2022
    cty_hiv_90_99_merge <- cty_hiv_90_99[,1:11]
    cty_hiv_90_99_merge$Raw_Value <- cty_hiv_90_99$hiv_adj
    
    # Merge HIV data frames
    cty_hiv_temp <- cty_hiv[which(cty_hiv$Age<80),]
    cty_hiv <- rbind(cty_hiv_temp,cty_hiv_80_89_merge,cty_hiv_90_99_merge)
    cty_hiv <- cty_hiv[order(cty_hiv$UID,cty_hiv$Runtype,cty_hiv$Scenario,cty_hiv$HIV,cty_hiv$Age,cty_hiv$Year,cty_hiv$TB),]
    
    ## Define TB states for prevalent TB for DALY calculations
    ## Dc is clinical TB, T is on-treatment, prevalent TB is Dc+T
    
    hiv_list <- unique(cty_hiv$HIV)
    
    if (length(hiv_list)==1) {
      tb_hiv_neg_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                      &cty_hiv$HIV=="HIV-"),]
    } else {
      tb_hiv_neg_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                      &cty_hiv$HIV=="HIV-"),]
      tb_hiv_pos_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                      &cty_hiv$HIV=="HIV1"),]
      tb_hiv_art_sub <- cty_hiv[which((cty_hiv$TB=="Dc"|cty_hiv$TB=="T")
                                      &cty_hiv$HIV=="ART1"),]
      hiv_pos_sub <- cty_hiv[which((cty_hiv$TB=="Lf"|cty_hiv$TB=="Ls"|cty_hiv$TB=="Ds"|cty_hiv$TB=="Un"|cty_hiv$TB=="Uc"|cty_hiv$TB=="R")
                                   &cty_hiv$HIV=="HIV1"),]
      hiv_art_sub <- cty_hiv[which((cty_hiv$TB=="Lf"|cty_hiv$TB=="Ls"|cty_hiv$TB=="Ds"|cty_hiv$TB=="Un"|cty_hiv$TB=="Uc"|cty_hiv$TB=="R")
                                   &cty_hiv$HIV=="ART1"),]
      art_sub <- cty_hiv[which(cty_hiv$HIV=="ART1"),]
    }
    
    cty <- cty[order(cty$Runtype,cty$Scenario,cty$UID,cty$Year,cty$Age),]
    cty_deaths <- cty_deaths[order(cty_deaths$Runtype,cty_deaths$Scenario,cty_deaths$UID,cty_deaths$Year,cty_deaths$Age),]
    cty_hiv <- cty_hiv[order(cty_hiv$Runtype,cty_hiv$Scenario,cty_hiv$UID,cty_hiv$Year,cty_hiv$Age),]
    
    ## Aggregate TB states for prevalent TB
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
    
    # Grab list of vaccine/coverage delivery scenarios
    
    scenarios <- unique(cty$Runtype)
    
    # Increase econ parameter set counter
    counter <- counter + 1
    
    ### Country life expectancy to single year ages
    le_under1 <- as.data.frame(lex_cty[1,])
    le_1to4 <- rbind(lex_cty[2,],lex_cty[2,],lex_cty[2,],lex_cty[2,])
    le_5to99 <- as.data.frame(lex_cty[rep(seq_len(nrow(lex_cty)),each=5),])
    le_5to99 <- as.data.frame(le_5to99[11:nrow(le_5to99),])
    colnames(le_under1) <- "ex"
    colnames(le_1to4) <- "ex"
    colnames(le_5to99) <- "ex"
    age_le <- as.data.frame(seq(0,99,1))
    colnames(age_le) <- "Age"
    le_single <- rbind(le_under1,le_1to4,le_5to99)
    le_single <- cbind(age_le,le_single)
    
    # third loop: delivery scenario
    # calc all results then separate step for summary statistics
    
    for (k0 in 1:length(scenarios)) {
      
      k <- scenarios[k0]
      
      disc_year <- 2028 #first year of vaccine introduction
      if (grepl("optimistic",k)==TRUE) {
        disc_year <- 2025
      }
      
      inc_scen <- cty[which(cty$Runtype==k),]
      deaths_scen <- cty_deaths[which(cty_deaths$Runtype==k),]
      tb_hiv_neg_scen <- tb_hiv_neg_uid[which(tb_hiv_neg_uid$Runtype==k),]
      
      if(exists('tb_hiv_pos_uid')) {tb_hiv_pos_scen <- tb_hiv_pos_uid[which(tb_hiv_pos_uid$Runtype==k),]}
      if(exists('tb_hiv_art_uid')) {tb_hiv_art_scen <- tb_hiv_art_uid[which(tb_hiv_art_uid$Runtype==k),]}
      if(exists('hiv_pos_uid')) {hiv_pos_scen <- hiv_pos_uid[which(hiv_pos_uid$Runtype==k),]}
      if(exists('hiv_art_uid')) {hiv_art_scen <- hiv_art_uid[which(hiv_art_uid$Runtype==k),]}
      if(exists('art_uid')) {art_scen <- art_uid[which(art_uid$Runtype==k),]}
      
      inc_cov <- inc_scen[which(inc_scen$Year>=start_year&inc_scen$Year<=end_year),]
      deaths_cov <- deaths_scen[which(deaths_scen$Year>=start_year&deaths_scen$Year<=end_year),]
      tb_hiv_neg_cov <- tb_hiv_neg_scen[which(tb_hiv_neg_scen$Year>=start_year&tb_hiv_neg_scen$Year<=end_year),]
      
      if(exists('tb_hiv_pos_scen')) {tb_hiv_pos_cov <- tb_hiv_pos_scen[which(tb_hiv_pos_scen$Year>=start_year&tb_hiv_pos_scen$Year<=end_year),]}
      if(exists('tb_hiv_art_scen')) {tb_hiv_art_cov <- tb_hiv_art_scen[which(tb_hiv_art_scen$Year>=start_year&tb_hiv_art_scen$Year<=end_year),]}
      if(exists('hiv_pos_scen')) {hiv_pos_cov <- hiv_pos_scen[which(hiv_pos_scen$Year>=start_year&hiv_pos_scen$Year<=end_year),]}
      if(exists('hiv_art_scen')) {hiv_art_cov <- hiv_art_scen[which(hiv_art_scen$Year>=start_year&hiv_art_scen$Year<=end_year),]}
      if(exists('art_scen')) {art_cov <- art_scen[which(art_scen$Year>=start_year&art_scen$Year<=end_year),]} #~# EDITED 12-Sep-2022
      
      # Merge YLL working data frame
      YLL_working <- merge(ledf_l,deaths_cov,by=c("Year","Age"))
      YLL_working <- YLL_working[order(YLL_working$Year,YLL_working$Age),]
      # Raw YLLs
      YLL_working$YLL_raw <- YLL_working$ALLdeaths*YLL_working$CLE
      # Discount YLLs to time of death using continuous time discounting
      YLL_working$YLL_dtod <- YLL_working$ALLdeaths*((1 - exp(-r * YLL_working$CLE)) / r)
      # Sum YLLs across ages
      YLL_year <- aggregate(YLL_working$YLL_raw~YLL_working$Year,FUN=sum)[,2]
      YLL_year_d <- aggregate(YLL_working$YLL_dtod~YLL_working$Year,FUN=sum)[,2]
      output_year$YLL_und <- YLL_year
      output_year$YLL_year_d <- YLL_year_d
      
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
      YLD_year_temp <- aggregate(tb_hiv_neg_cov_sub$Raw_Value~tb_hiv_neg_cov_sub$Year,FUN=sum)[,2]
      
      if(exists('tb_hiv_pos_cov')) {YLD_year_tb_hiv_pos <- aggregate(tb_hiv_pos_cov$Raw_Value~tb_hiv_pos_cov$Year,FUN=sum)[,2]}
      if(exists('tb_hiv_art_cov')) {YLD_year_tb_hiv_art <- aggregate(tb_hiv_art_cov$Raw_Value~tb_hiv_art_cov$Year,FUN=sum)[,2]}
      if(exists('hiv_pos_cov')) {YLD_year_hiv_pos <- aggregate(hiv_pos_cov$Raw_Value~hiv_pos_cov$Year,FUN=sum)[,2]}
      if(exists('hiv_art_cov')) {YLD_year_hiv_art <- aggregate(hiv_art_cov$Raw_Value~hiv_art_cov$Year,FUN=sum)[,2]}
      if(exists('art_cov')) {YLD_year_art <- aggregate(art_cov$Raw_Value~art_cov$Year,FUN=sum)[,2]}
      
      ifelse(exists('YLD_year_pos'),
             YLD_year <- YLD_year_temp * dwTB[counter,1] + YLD_year_tb_hiv_pos * dwTBHIV[counter,1] +
               YLD_year_tb_hiv_art * dwTBHIV[counter,1] + YLD_year_hiv_pos * dwHIV[counter,1] +
               YLD_year_hiv_art * dwART[counter,1],
             YLD_year <- YLD_year_temp * dwTB[counter,1])
      
      output_year$YLD_und <- YLD_year
      
      ## Estimate discounted and undiscounted YLLs and YLDs using discrete time discounting
      for (m in 1:nrow(output_year)) {
        if (output_year$Year[m]<disc_year) {
          output_year[m,"YLL_d"] <- output_year[m,"YLL_year_d"]
        } else {
          output_year[m,"YLL_d"] <- output_year[m,"YLL_year_d"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"YLD_d"] <- output_year[m,"YLD_und"]
        } else {
          output_year[m,"YLD_d"] <- output_year[m,"YLD_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
      }
      
      # Sum YLLs + YLDs = DALYs
      output_year$dalys_und <- output_year$YLL_und + output_year$YLD_und
      output_year$dalys_d <- output_year$YLL_d + output_year$YLD_d
      
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
      if (k=="baseline") {
        vax_year[,2] <- 0
      } else {
        for (m in 1:nrow(vax_year)) {
          if (vax_year[m,1]==0) {
            vax_year[m,2] <- 0
          }
          else {
            vax_year[m,2] <- vax_year[m-1,2]+1
          }
          
        }
      }
      
      ## Calculate testing and treatment costs by scenario, DS/RR
      output_year$test_ds_und <- notif_DS_year*test_ds_cty[,counter+4]
      output_year$test_rr_und <- notif_RR_year*test_rr_cty[,counter+4]
      output_year$treat_ds_und <- notif_DS_year*treat_ds_cty[,counter+4]
      output_year$treat_rr_und <- notif_RR_year*treat_rr_cty[,counter+4]
      
      ## Calculate ART costs
      if(exists('hiv_art_year')) {
        art_und <- hiv_art_year*art_cty[,counter+4]
        output_year$art_und <- art_und
      }
      
      #~~
      ## Calculate non-medical costs by scenario, DS/RR
      output_year$nm_ds_und <- notif_DS_year*nm_ds_cty[,counter+4]
      output_year$nm_rr_und <- notif_RR_year*nm_rr_cty[,counter+4]
      
      #~~
      ## Calculate indirect costs
      output_year$ind_ds_und <- notif_DS_year*ind_ds_cty[,counter+4]
      output_year$ind_rr_und <- notif_RR_year*ind_rr_cty[,counter+4]
      
      # Calculate productivity costs
      output_year$prod_cost_und <- output_year$LEX_year_d*base_cty$GDPpc
      
      ## Calculate vaccine costs by scenario, including introduction costs in year 1
      intro_pop <- inc_cov[which(inc_cov$Age==9),]
      if (k=="baseline") {
        output_year$vax_cost_und <- 0
      }
      if (grepl("Neo",k)==TRUE) {
        for (m in 1:nrow(vax_year)) {
          if (vax_year[m,2]==0) {
            output_year$vax_cost_und[m] <- 0
          }
          if (vax_year[m,2]==1) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_inf + vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_inf_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]>1) {
            output_year$vax_cost_und[m] <- (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_inf_cty[,counter+4])))*(1+wastage)
          }
        }
      }
      if (grepl("AdoAdu",k)==TRUE) {
        for (m in 1:nrow(vax_year)) {
          if (vax_year[m,2]==0) {
            output_year$vax_cost_und[m] <- 0
          }
          if (vax_year[m,2]==1) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado)*0.2 + (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]==2) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado)*0.2 + (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]==3) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado)*0.2 + (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]==4) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado)*0.2 + (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]==5) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado)*0.2 + (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]>5) {
            output_year$vax_cost_und[m] <- (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
        }
      }
      if (grepl("pessimistic",k)==TRUE) {
        for (m in 1:nrow(vax_year)) {
          if (vax_year[m,2]==0) {
            output_year$vax_cost_und[m] <- 0
          }
          if (vax_year[m,2]==1) {
            output_year$vax_cost_und[m] <- (intro_pop$Population[m]*vax_intro_ado + vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
          if (vax_year[m,2]>5) {
            output_year$vax_cost_und[m] <- (vax_year[m,1]*(vax_price+vax_supply+as.numeric(costdel_ado_cty[,counter+4])))*(1+wastage)
          }
        }
      }
      #~#
      
      ## Estimate discounted and undiscounted costs for all cost categories
      for (m in 1:nrow(output_year)) {
        if (output_year$Year[m]<disc_year) {
          output_year[m,"test_ds_d"] <- output_year[m,"test_ds_und"]
        } else {
          output_year[m,"test_ds_d"] <- output_year[m,"test_ds_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"test_rr_d"] <- output_year[m,"test_rr_und"]
        } else {
          output_year[m,"test_rr_d"] <- output_year[m,"test_rr_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"treat_ds_d"] <- output_year[m,"treat_ds_und"]
        } else {
          output_year[m,"treat_ds_d"] <- output_year[m,"treat_ds_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"treat_rr_d"] <- output_year[m,"treat_rr_und"]
        } else {
          output_year[m,"treat_rr_d"] <- output_year[m,"treat_rr_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if(exists('art_und')) {
          if (output_year$Year[m]<disc_year) {
            output_year[m,"art_d"] <- output_year[m,"art_und"]
          } else {
            output_year[m,"art_d"] <- output_year[m,"art_und"] / (1+r)^(output_year$Year[m]-disc_year)
          }
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"nm_ds_d"] <- output_year[m,"nm_ds_und"]
        } else {
          output_year[m,"nm_ds_d"] <- output_year[m,"nm_ds_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"nm_rr_d"] <- output_year[m,"nm_rr_und"]
        } else {
          output_year[m,"nm_rr_d"] <- output_year[m,"nm_rr_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"ind_ds_d"] <- output_year[m,"ind_ds_und"]
        } else {
          output_year[m,"ind_ds_d"] <- output_year[m,"ind_ds_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"ind_rr_d"] <- output_year[m,"ind_rr_und"]
        } else {
          output_year[m,"ind_rr_d"] <- output_year[m,"ind_rr_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"prod_cost_d"] <- output_year[m,"prod_cost_und"]
        } else {
          output_year[m,"prod_cost_d"] <- output_year[m,"prod_cost_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
        if (output_year$Year[m]<disc_year) {
          output_year[m,"vax_cost_d"] <- output_year[m,"vax_cost_und"]
        } else {
          output_year[m,"vax_cost_d"] <- output_year[m,"vax_cost_und"] / (1+r)^(output_year$Year[m]-disc_year)
        }
      }
      
      ## Collate DALYs and each cost category by country, year, UID, delivery scenario for undiscounted/discounted
      ifelse(exists('art_und'),
             output_sum <- as.data.frame(cbind(base_cty$country,
                                               i,
                                               base_cty$region,
                                               base_cty$income,
                                               output_year$Year,
                                               j,
                                               k,
                                               output_year$dalys_und,
                                               output_year$dalys_d,
                                               output_year$test_ds_und+output_year$test_rr_und+output_year$treat_ds_und+output_year$treat_rr_und+output_year$art_und+output_year$nm_ds_und+output_year$nm_rr_und+output_year$ind_ds_und+output_year$ind_rr_und+output_year$prod_cost_und,
                                               output_year$vax_cost_und,
                                               output_year$test_ds_und+output_year$test_rr_und+output_year$treat_ds_und+output_year$treat_rr_und+output_year$art_und+output_year$nm_ds_und+output_year$nm_rr_und+output_year$ind_ds_und+output_year$ind_rr_und+output_year$prod_cost_und+output_year$vax_cost_und,
                                               output_year$test_ds_d+output_year$test_rr_d+output_year$treat_ds_d+output_year$treat_rr_d+output_year$art_d+output_year$nm_ds_d+output_year$nm_rr_d+output_year$ind_ds_d+output_year$ind_rr_d+output_year$prod_cost_d,
                                               output_year$vax_cost_d,
                                               output_year$test_ds_d+output_year$test_rr_d+output_year$treat_ds_d+output_year$treat_rr_d+output_year$art_d+output_year$nm_ds_d+output_year$nm_rr_d+output_year$ind_ds_d+output_year$ind_rr_d+output_year$prod_cost_d+output_year$vax_cost_d)),
             output_sum <- as.data.frame(cbind(base_cty$country,
                                               i,
                                               base_cty$region,
                                               base_cty$income,
                                               output_year$Year,
                                               j,
                                               k,
                                               output_year$dalys_und,
                                               output_year$dalys_d,
                                               output_year$test_ds_und+output_year$test_rr_und+output_year$treat_ds_und+output_year$treat_rr_und+output_year$nm_ds_und+output_year$nm_rr_und+output_year$ind_ds_und+output_year$ind_rr_und+output_year$prod_cost_und,
                                               output_year$vax_cost_und,
                                               output_year$test_ds_und+output_year$test_rr_und+output_year$treat_ds_und+output_year$treat_rr_und+output_year$nm_ds_und+output_year$nm_rr_und+output_year$ind_ds_und+output_year$ind_rr_und+output_year$prod_cost_und+output_year$vax_cost_und,
                                               output_year$test_ds_d+output_year$test_rr_d+output_year$treat_ds_d+output_year$treat_rr_d+output_year$nm_ds_d+output_year$nm_rr_d+output_year$ind_ds_d+output_year$ind_rr_d+output_year$prod_cost_d,
                                               output_year$vax_cost_d,
                                               output_year$test_ds_d+output_year$test_rr_d+output_year$treat_ds_d+output_year$treat_rr_d+output_year$nm_ds_d+output_year$nm_rr_d+output_year$ind_ds_d+output_year$ind_rr_d+output_year$prod_cost_d+output_year$vax_cost_d)))
      colnames(output_sum) <- c("country","code","region","income","year","UID","scenario","dalys_und","dalys_d","disease_cost_und","vax_cost_und","cost_und","disease_cost_d","vax_cost_d","cost_d")
      
      ## Append to overall output dataframe
      output_all <- rbind(output_all,output_sum)
      
      ## Collate DALYs and each cost category by country, year, UID, delivery scenario for undiscounted/discounted
      ifelse(exists('art_und'),
             output_disaggregated_sum <- as.data.frame(cbind(base_cty$country,i,base_cty$region,base_cty$income,output_year$Year,j,k,
                                                             output_year$test_ds_und,output_year$test_rr_und,output_year$treat_ds_und,output_year$treat_rr_und,
                                                             output_year$nm_ds_und,output_year$nm_rr_und,output_year$ind_ds_und,output_year$ind_rr_und,output_year$prod_cost_und,
                                                             output_year$art_und,output_year$vax_cost_und,
                                                             output_year$test_ds_d,output_year$test_rr_d,output_year$treat_ds_d,output_year$treat_rr_d,
                                                             output_year$nm_ds_d,output_year$nm_rr_d,output_year$ind_ds_d,output_year$ind_rr_d,output_year$prod_cost_d,
                                                             output_year$art_d,output_year$vax_cost_d)),
             output_disaggregated_sum <- as.data.frame(cbind(base_cty$country,i,base_cty$region,base_cty$income,output_year$Year,j,k,
                                                             output_year$test_ds_und,output_year$test_rr_und,output_year$treat_ds_und,output_year$treat_rr_und,
                                                             output_year$nm_ds_und,output_year$nm_rr_und,output_year$ind_ds_und,output_year$ind_rr_und,output_year$prod_cost_und,
                                                             output_year$vax_cost_und,
                                                             output_year$test_ds_d,output_year$test_rr_d,output_year$treat_ds_d,output_year$treat_rr_d,
                                                             output_year$nm_ds_d,output_year$nm_rr_d,output_year$ind_ds_d,output_year$ind_rr_d,output_year$prod_cost_d,
                                                             output_year$vax_cost_d)))
      ifelse(exists('art_und'),
             colnames(output_disaggregated_sum) <- c("country","code","region","income","year","UID","scenario","test_ds_und","test_rr_und","treat_ds_und","treat_rr_und","nm_ds_und","nm_rr_und","ind_ds_und","ind_rr_und","prod_cost_und","art_und","vax_cost_und","test_ds_d","test_rr_d","treat_ds_d","treat_rr_d","nm_ds_d","nm_rr_d","ind_ds_d","ind_rr_d","prod_cost_d","art_d","vax_cost_d"),
             colnames(output_disaggregated_sum) <- c("country","code","region","income","year","UID","scenario","test_ds_und","test_rr_und","treat_ds_und","treat_rr_und","nm_ds_und","nm_rr_und","ind_ds_und","ind_rr_und","prod_cost_und","vax_cost_und","test_ds_d","test_rr_d","treat_ds_d","treat_rr_d","nm_ds_d","nm_rr_d","ind_ds_d","ind_rr_d","prod_cost_d","vax_cost_d"))
      
      ## Append to overall output_disaggregated dataframe
      output_disaggregated_all <- rbind(output_disaggregated_all,output_disaggregated_sum)
      
    }
    
    
  }
  
  
  ## Save output for each country
  setwd("") # CHANGE: Specify working directory for output
  write.csv(output_all,paste0(i,"_soc_cea.csv"),row.names=FALSE)
  rm(output_sum)
  
  write.csv(output_disaggregated_all,paste0(i,"_disaggregated_soc.csv"),row.names=FALSE)
  rm(output_disaggregated_sum)

}