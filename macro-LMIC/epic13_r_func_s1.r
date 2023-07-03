index<-as.integer(commandArgs(trailingOnly=TRUE)[1])
tbmacro <- function(index){

  ###################################################
  # CIPE model
  # Jean-Louis Arcand, September 2021
  # arcandjl@alum.mit.edu
  ###################################################

  library(tidyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(dplyr)
  library(stringr)
  library(forcats)
  library(countrycode)
  library(arrow)
  library(pwt10)
  # Load Penn World Table v10
  # Latest version available at https://www.rug.nl/ggdc/productivity/pwt/
  # Since PWT is available in R, one can load it directly
  # using the appropriate R package
  data("pwt10.0")
  # In theory: 183 countries available
  library(WDI)
  library(Rilostat)
  library(rlist)
  library(data.table)
  library(ggplot2)

  # List of countries for simulations
  lcountries <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/LMIC.csv",header=TRUE,check.names=FALSE)[,2]
  lcountries_full <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/LMIC.csv",header=TRUE,check.names=FALSE)

  ## Load WDI data with GDP in constant 2020 dollars
  wdi <- read.csv("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/gdp.csv",header=TRUE,check.names=FALSE)

  ## Load up NEW country macro variables
  load("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/new_macro_inputs_Nov-6-2022.rData") ## edited ACP 1-Nov-2022
  # macro_par_means, macro_par_resid

  # Helper function for inverse logit
  inverse_logit <- function(x) 1/(1+exp(-x)) ## edited NAM Aug-25

  # Extract WDI GDP data for countries w/o 2019/2020 GDP
  # Here we use CURRENT $ GDP to match EPI vaccination costs
  wdifull <- WDI(
    country = c("ERI","SSD","SYR","VEN"),
    indicator = c("GDP" = "NY.GDP.MKTP.CD"),
    start = 2010, end = 2020, extra = TRUE
  ) %>% select(year, iso3c, GDP) %>%
    # Stupid problem with the change of the official name of Turkey!
    # mutate(country = replace(country, country == "Turkiye", "Turkey")) %>%
    # mutate(iso3c = replace(iso3c, country == "Turkey", "TUR")) %>%
    drop_na() %>%
    group_by(iso3c) %>% filter(year > max(year)-2) %>% ungroup()
  # Keep the 2 most recent observations for initial GDP calibration inside function
  # For 101 countries, these will indeed be 2020 and 2019
  # The exception are given by:
  # ERI: 2011 and 2010
  # SSD: 2015 and 2014
  # VEN: 2014 and 2013
  # SYR: 2018 and 2017

  sdatafull0 <- WDI(
    country = c(lcountries),
    indicator = c("s" = "NE.GDI.TOTL.ZS"),
    start = 2008, end = 2019, extra = TRUE
  )  %>% select(year, country, iso3c, s) %>%
    # Stupid problem with the change of the official name of Turkey!
    mutate(country = replace(country, country == "Turkiye", "Turkey")) %>%
    mutate(iso3c = replace(iso3c, country == "Turkey", "TUR")) %>%
    # Eritrea has s missing from 2012 to 2019
    # Iran has s missing for 2019
    # Laos has s missing for 2017-2019
    # Liberia has s missing for all years
    # Maldives has s missing for 2008-2013
    # Myanmar has s missing for 2008
    # Malawi has s missing for all years
    # Papua New Guinea has s missing for all years
    # South Sudan has s missing for 2016-2019
    # Sao Tome e Principe has s missing for all years
    # Ethiopia has s = 0 for 2008-2019 so replace with NA
  # Suriname has s missing for 2011-2019
  # Syria has s missing for all years
  # Venezuela has s missing for 2015-2019
  # Vietnam has s missing for all years
  # Yemen has s missing for all years
  # Zambia has s missing for 2008-2009
  mutate(s = replace(s, s == 0, NA)) %>%
    arrange(iso3c, year)

  # Replacement countries for s
  # Relatively few missing datapoints for s so use this as the starting point
  sdatafull0$s[sdatafull0$iso3c == "AFG"] <- sdatafull0$s[sdatafull0$iso3c == "TJK"]
  sdatafull0$s[sdatafull0$iso3c == "LBR"] <- sdatafull0$s[sdatafull0$iso3c == "SLE"]
  sdatafull0$s[sdatafull0$iso3c == "MWI"] <- sdatafull0$s[sdatafull0$iso3c == "ZMB"]
  sdatafull0$s[sdatafull0$iso3c == "PNG"] <- sdatafull0$s[sdatafull0$iso3c == "IDN"]
  sdatafull0$s[sdatafull0$iso3c == "STP"] <- sdatafull0$s[sdatafull0$iso3c == "CMR"]
  sdatafull0$s[sdatafull0$iso3c == "SYR"] <- sdatafull0$s[sdatafull0$iso3c == "JOR"]
  sdatafull0$s[sdatafull0$iso3c == "VNM"] <- sdatafull0$s[sdatafull0$iso3c == "THA"]
  sdatafull0$s[sdatafull0$iso3c == "YEM"] <- sdatafull0$s[sdatafull0$iso3c == "JOR"]

  # After country substitution for s

  # ETH has s missing for 2008-2010
  # IRN has s missing for 2019
  # LAO has s missing for 2017-2019
  # MDV has s missing for 2008-2013
  # MMR has s missing for 2008
  # MWI and ZMB, which is the replacement, have s missing for 2008-2009

  # We will be sampling for s, so as long as we can calculate a reasonable
  # mean and an SD we can afford to omit some years (including the latest)
  sdatafull <- sdatafull0 %>% drop_na(s)
  # So after dropping the observations with missing s we have:
  # ERI has s missing for 2012-2019
  # SSD has s missing for 2016-2019
  # SUR has s missing for 2011-2019
  # VEN has s missing for 2015-2019
  # We will therefore have to rely on earlier years for these countries

  ## Replace s data using neighboring country for those lacking data
  # sdatafull <- sdatafull[order(sdatafull$country,sdatafull$year),]
  #
  # temp <- sdatafull[sdatafull$iso3c == "ETH"&sdatafull$year>2011,]
  # temp$country <- "Eritrea"
  # temp$iso3c <- "ERI"
  # sdatafull <- rbind(sdatafull,temp)
  #
  # temp <- sdatafull[sdatafull$iso3c == "SDN"&sdatafull$year>2015,]
  # temp$country <- "South Sudan"
  # temp$iso3c <- "SSD"
  # sdatafull <- rbind(sdatafull,temp)
  #
  # temp <- sdatafull[sdatafull$iso3c == "DOM"&sdatafull$year>2010,]
  # temp$country <- "Suriname"
  # temp$iso3c <- "SUR"
  # sdatafull <- rbind(sdatafull,temp)
  #
  # temp <- sdatafull[sdatafull$iso3c == "COL"&sdatafull$year>2014,]
  # temp$country <- "Venezuela, RB"
  # temp$iso3c <- "VEN"
  # sdatafull <- rbind(sdatafull,temp)
  #
  # sdatafull <- sdatafull[order(sdatafull$country,sdatafull$year),]

  # Merge PWT data into WDI data for easy country substitution later
  pwtfull <- pwt10.0 %>%
    select(year, country, isocode, rtfpna, delta, hc) %>%
    # Select years over which to estimate key parameters: alpha, s, g, delta
    filter(
      year >= 2008,
      year <= 2019
    )

  # Merge PWT data into WDI data for easy country substitution later
  pwtfull$iso3c <- pwtfull$isocode
  datam <- merge(x=sdatafull, y=pwtfull, by = c("iso3c","year"), all.x=TRUE) %>%
    arrange(iso3c, year) %>% select(-c(country.y, isocode)) %>%
    rename(country = country.x)

  # Labor share data from ILOStat
  alphadata <- get_ilostat(
    id = c("LAP_2GDP_NOC_RT_A"),
    filters = list(ref_area = lcountries),
    quiet = TRUE
  )
  alphadata$alpha <- 1 - (alphadata$obs_value/100)
  alphadata <- alphadata %>%rename(iso3c = ref_area, year = time) %>%
    select(c(iso3c, year, alpha))

  # Merge all three datasets together so as to see precisely what is missing
  datamerge <- merge(x=datam, y=alphadata, by = c("iso3c","year"), all.x=TRUE) %>%
    arrange(iso3c, year)

  # Replace PWT data using neighboring/similar country IN dataset for those lacking it
  # Leads to a few changes with respect to our earlier list of substitutions

  datamerge$rtfpna[datamerge$iso3c == "AFG"] <- datamerge$rtfpna[datamerge$iso3c == "TJK"]
  datamerge$delta[datamerge$iso3c == "AFG"] <- datamerge$delta[datamerge$iso3c == "TJK"]
  datamerge$hc[datamerge$iso3c == "AFG"] <- datamerge$hc[datamerge$iso3c == "TJK"]

  datamerge$rtfpna[datamerge$iso3c == "ALB"] <- datamerge$rtfpna[datamerge$iso3c == "SRB"]

  datamerge$rtfpna[datamerge$iso3c == "AZE"] <- datamerge$rtfpna[datamerge$iso3c == "ARM"]
  datamerge$hc[datamerge$iso3c == "AZE"] <- datamerge$hc[datamerge$iso3c == "ARM"]

  datamerge$rtfpna[datamerge$iso3c == "BGD"] <- datamerge$rtfpna[datamerge$iso3c == "IND"]

  datamerge$rtfpna[datamerge$iso3c == "BLR"] <- datamerge$rtfpna[datamerge$iso3c == "UKR"]
  datamerge$hc[datamerge$iso3c == "BLR"] <- datamerge$hc[datamerge$iso3c == "UKR"]

  datamerge$rtfpna[datamerge$iso3c == "BTN"] <- datamerge$rtfpna[datamerge$iso3c == "IND"]
  datamerge$hc[datamerge$iso3c == "BTN"] <- datamerge$hc[datamerge$iso3c == "IND"]

  datamerge$rtfpna[datamerge$iso3c == "CUB"] <- datamerge$rtfpna[datamerge$iso3c == "DOM"]
  datamerge$delta[datamerge$iso3c == "CUB"] <- datamerge$delta[datamerge$iso3c == "DOM"]
  datamerge$hc[datamerge$iso3c == "CUB"] <- datamerge$hc[datamerge$iso3c == "DOM"]

  datamerge$rtfpna[datamerge$iso3c == "ERI"][1:4] <- datamerge$rtfpna[datamerge$iso3c == "SDN"][1:4]
  datamerge$delta[datamerge$iso3c == "ERI"][1:4] <- datamerge$delta[datamerge$iso3c == "SDN"][1:4]
  datamerge$hc[datamerge$iso3c == "ERI"][1:4] <- datamerge$hc[datamerge$iso3c == "SDN"][1:4]
  # # Only 4 years available for s for Eritrea

  # datamerge$rtfpna[datamerge$iso3c == "ERI"] <- datamerge$rtfpna[datamerge$iso3c == "SDN"]
  # datamerge$delta[datamerge$iso3c == "ERI"] <- datamerge$delta[datamerge$iso3c == "SDN"]
  # datamerge$hc[datamerge$iso3c == "ERI"] <- datamerge$hc[datamerge$iso3c == "SDN"]

  datamerge$rtfpna[datamerge$iso3c == "ETH"][1:9] <- datamerge$rtfpna[datamerge$iso3c == "KEN"][4:12]
  # Only 2011-2019 for Ethiopia

  datamerge$rtfpna[datamerge$iso3c == "GEO"] <- datamerge$rtfpna[datamerge$iso3c == "ARM"]
  datamerge$hc[datamerge$iso3c == "GEO"] <- datamerge$hc[datamerge$iso3c == "ARM"]

  datamerge$rtfpna[datamerge$iso3c == "GHA"] <- datamerge$rtfpna[datamerge$iso3c == "CIV"]

  datamerge$rtfpna[datamerge$iso3c == "GIN"] <- datamerge$rtfpna[datamerge$iso3c == "CIV"]
  datamerge$hc[datamerge$iso3c == "GIN"] <- datamerge$hc[datamerge$iso3c == "CIV"]

  datamerge$rtfpna[datamerge$iso3c == "GMB"] <- datamerge$rtfpna[datamerge$iso3c == "SEN"]

  datamerge$rtfpna[datamerge$iso3c == "GNQ"] <- datamerge$rtfpna[datamerge$iso3c == "CMR"]
  datamerge$hc[datamerge$iso3c == "GNQ"] <- datamerge$hc[datamerge$iso3c == "CMR"]

  datamerge$rtfpna[datamerge$iso3c == "KHM"] <- datamerge$rtfpna[datamerge$iso3c == "THA"]

  datamerge$rtfpna[datamerge$iso3c == "LBR"] <- datamerge$rtfpna[datamerge$iso3c == "SLE"]

  datamerge$rtfpna[datamerge$iso3c == "LBY"] <- datamerge$rtfpna[datamerge$iso3c == "TUN"]
  datamerge$delta[datamerge$iso3c == "LBY"] <- datamerge$delta[datamerge$iso3c == "TUN"]
  datamerge$hc[datamerge$iso3c == "LBY"] <- datamerge$hc[datamerge$iso3c == "TUN"]

  datamerge$rtfpna[datamerge$iso3c == "MDG"] <- datamerge$rtfpna[datamerge$iso3c == "MOZ"]

  datamerge$rtfpna[datamerge$iso3c == "MDV"][1:6] <- datamerge$rtfpna[datamerge$iso3c == "FJI"][7:12]
  # Only 2014-2019 available for The Maldives

  datamerge$rtfpna[datamerge$iso3c == "MLI"] <- datamerge$rtfpna[datamerge$iso3c == "BFA"]

  datamerge$rtfpna[datamerge$iso3c == "MMR"][1:11] <- datamerge$rtfpna[datamerge$iso3c == "THA"][2:12]
  # No 2008 data for Myanmar

  datamerge$rtfpna[datamerge$iso3c == "MNE"] <- datamerge$rtfpna[datamerge$iso3c == "SRB"]
  datamerge$hc[datamerge$iso3c == "MNE"] <- datamerge$hc[datamerge$iso3c == "SRB"]

  datamerge$rtfpna[datamerge$iso3c == "MWI"] <- datamerge$rtfpna[datamerge$iso3c == "ZMB"]

  datamerge$rtfpna[datamerge$iso3c == "NPL"] <- datamerge$rtfpna[datamerge$iso3c == "IND"]

  datamerge$rtfpna[datamerge$iso3c == "PAK"] <- datamerge$rtfpna[datamerge$iso3c == "IND"]

  datamerge$rtfpna[datamerge$iso3c == "PNG"] <- datamerge$rtfpna[datamerge$iso3c == "IDN"]
  datamerge$delta[datamerge$iso3c == "PNG"] <- datamerge$delta[datamerge$iso3c == "IDN"]
  datamerge$hc[datamerge$iso3c == "PNG"] <- datamerge$hc[datamerge$iso3c == "IDN"]

  datamerge$rtfpna[datamerge$iso3c == "SLB"] <- datamerge$rtfpna[datamerge$iso3c == "FJI"]
  datamerge$delta[datamerge$iso3c == "SLB"] <- datamerge$delta[datamerge$iso3c == "FJI"]
  datamerge$hc[datamerge$iso3c == "SLB"] <- datamerge$hc[datamerge$iso3c == "FJI"]

  datamerge$rtfpna[datamerge$iso3c == "SLV"] <- datamerge$rtfpna[datamerge$iso3c == "NIC"]

  datamerge$rtfpna[datamerge$iso3c == "SSD"][1:8] <- datamerge$rtfpna[datamerge$iso3c == "SDN"][1:8]
  datamerge$delta[datamerge$iso3c == "SSD"][1:8] <- datamerge$delta[datamerge$iso3c == "SDN"][1:8]
  datamerge$hc[datamerge$iso3c == "SSD"][1:8] <- datamerge$hc[datamerge$iso3c == "SDN"][1:8]
  # # Only 2008-2015 for South Sudan

  # datamerge$rtfpna[datamerge$iso3c == "SSD"] <- datamerge$rtfpna[datamerge$iso3c == "SDN"]
  # datamerge$delta[datamerge$iso3c == "SSD"] <- datamerge$delta[datamerge$iso3c == "SDN"]
  # datamerge$hc[datamerge$iso3c == "SSD"] <- datamerge$hc[datamerge$iso3c == "SDN"]

  datamerge$rtfpna[datamerge$iso3c == "STP"] <- datamerge$rtfpna[datamerge$iso3c == "CMR"]
  datamerge$hc[datamerge$iso3c == "STP"] <- datamerge$hc[datamerge$iso3c == "CMR"]

  datamerge$rtfpna[datamerge$iso3c == "SUR"][1:3] <- datamerge$rtfpna[datamerge$iso3c == "DOM"][1:3]
  datamerge$hc[datamerge$iso3c == "SUR"][1:3] <- datamerge$hc[datamerge$iso3c == "DOM"][1:3]
  # # Only 2008-2010 for Suriname

  # datamerge$rtfpna[datamerge$iso3c == "SUR"] <- datamerge$rtfpna[datamerge$iso3c == "DOM"]
  # datamerge$hc[datamerge$iso3c == "SUR"] <- datamerge$hc[datamerge$iso3c == "DOM"]

  datamerge$rtfpna[datamerge$iso3c == "SYR"] <- datamerge$rtfpna[datamerge$iso3c == "JOR"]

  datamerge$rtfpna[datamerge$iso3c == "TCD"] <- datamerge$rtfpna[datamerge$iso3c == "NER"]
  datamerge$hc[datamerge$iso3c == "TCD"] <- datamerge$hc[datamerge$iso3c == "NER"]

  datamerge$rtfpna[datamerge$iso3c == "TLS"] <- datamerge$rtfpna[datamerge$iso3c == "IDN"]
  datamerge$delta[datamerge$iso3c == "TLS"] <- datamerge$delta[datamerge$iso3c == "IDN"]
  datamerge$hc[datamerge$iso3c == "TLS"] <- datamerge$hc[datamerge$iso3c == "IDN"]

  datamerge$rtfpna[datamerge$iso3c == "UGA"] <- datamerge$rtfpna[datamerge$iso3c == "TZA"]

  datamerge$rtfpna[datamerge$iso3c == "UZB"] <- datamerge$rtfpna[datamerge$iso3c == "TJK"]
  datamerge$delta[datamerge$iso3c == "UZB"] <- datamerge$delta[datamerge$iso3c == "TJK"]
  datamerge$hc[datamerge$iso3c == "UZB"] <- datamerge$hc[datamerge$iso3c == "TJK"]

  datamerge$rtfpna[datamerge$iso3c == "VNM"] <- datamerge$rtfpna[datamerge$iso3c == "THA"]

  datamerge$rtfpna[datamerge$iso3c == "VUT"] <- datamerge$rtfpna[datamerge$iso3c == "FJI"]
  datamerge$delta[datamerge$iso3c == "VUT"] <- datamerge$delta[datamerge$iso3c == "FJI"]
  datamerge$hc[datamerge$iso3c == "VUT"] <- datamerge$hc[datamerge$iso3c == "FJI"]

  datamerge$rtfpna[datamerge$iso3c == "YEM"] <- datamerge$rtfpna[datamerge$iso3c == "JOR"]

  datamerge = datamerge %>% rename(countryname = country)

  # Construct growth rates of human capital (education) and TFP
  datamerge <- datamerge %>% group_by(iso3c) %>%  mutate(
    h = log(hc) - lag(log(hc)), # growth rate of human capital
    g = log(rtfpna) - lag(log(rtfpna)) # g directly from PWT
  ) %>% drop_na(h, g)
  # Can scratch back a few observations here.  Come back to this in next version
  # %>% drop_na(h, g) drops years (2008) for which we have no h and g data by construction

  # Load Rilostat library
  lfpr0full <- get_ilostat(
    id = c("EAP_DWAP_SEX_AGE_RT_A"),
    filters = list(
      ref_area = lcountries,
      classif1 = c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24",
                   "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34",
                   "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44",
                   "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54",
                   "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64",
                   "AGE_5YRBANDS_YGE65"),
      sex = c("T")
    ), quiet = TRUE
  )

  ################################
  # Population projection section
  ################################

  # outermost loop = country
  i <- lcountries[index]

  load("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/data/inputs_for_extrap_Aug-17-2022.rData") # edited NAM Aug-17
  mort_i <- mort[mort$iso3==i,c(2,3,5)]
  births_i <- births[births$iso3==i & births$year>2050,c(1,2,4)]

  # Initialize dataframe
  output_all <- NULL

  ## Subset descriptive dataframe to indexed country

  cty <- lcountries_full[which(lcountries_full$code==i),]

  ## Grab list of UIDs

  directory <- paste0("/n/holyscratch01/menzies_lab/Lab/EconOutputRedo/tbvax/econ_output/")

  UID_list <- list.files(path = paste0(directory,"cc_TB/",i,"_TB/"))
  UID_list <- tools::file_path_sans_ext(UID_list)

  ## Read in cost data (for all UIDs) # edited NAM Aug-13
  country_cost0 <- read.csv(paste0("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/outcomes/",i,"_outcomes_macro_s1.csv"),header=TRUE,check.names=FALSE)
  
  ## Read in inc YLD data (for all UIDs) # NAM edits Oct 31 2022
  load(paste0("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/yld/",i,"_yld.rData")) # country_inc_yld

  for (j0 in 1:length(UID_list)) { 

    j <- UID_list[j0]
    
    # Subset YLD data for single UID # NAM edits Nov 3 2022
    country_yld0 <- country_inc_yld[, dimnames(country_inc_yld)[[2]]==j, , ]

    # Read in the epi data
    country_TB <- read_parquet(paste0(directory,"cc_TB/",i,"_TB/",j,".parquet")) %>% collect()
    # Problems with additional factor levels for age categories in these files
    # So need to reinitialize the underlying levels of the factor
    country_TB$AgeGrp <- factor(country_TB$AgeGrp)

    # Define list of vaccine delivery scenarios # ACP edits Nov 3 2022
    scenarios <- c("basecase_med_10y_PRI_Neo","basecase_med_10y_PPI_AdoAdu")

    # Create country_TB_LB from country_TB
    country_TB_LB <- filter(country_TB, Runtype %in% "baseline") %>%
      select(Year, AgeGrp, Population)

    # Extend country_TB_LB to 2080 # edited NAM Aug-17
    ord_nam <- unique(country_TB_LB$AgeGrp)[c(c(82,1,12,23,34,45,56,67,78,80),(1:82)[-c(82,1,12,23,34,45,56,67,78,80)])]
    country_TB_LB2 <- expand.grid(ord_nam,2019:2100)[,2:1]
    names(country_TB_LB2) <- c("Year","AgeGrp")
    country_TB_LB2$Population <- NA
    # Fill in observed data
    for(ii in unique(country_TB_LB$Year)){
      for(jj in unique(country_TB_LB$AgeGrp)){
        country_TB_LB2[country_TB_LB2$Year==ii & country_TB_LB2$AgeGrp==jj,3] <-
          country_TB_LB[country_TB_LB$Year==ii & country_TB_LB$AgeGrp==jj,3]
      }
    }
    # Fill in new birth data
    country_TB_LB2$Population[country_TB_LB2$AgeGrp==ord_nam[1] & country_TB_LB2$Year>2050] <-
      as.numeric(births_i$births)/1e3

    # Apply mort rates
    for(ii in 2051:2100){ # ii=2051
      country_TB_LB2$Population[country_TB_LB2$Year==(ii)][2:80] <-
        country_TB_LB2$Population[country_TB_LB2$Year==(ii-1)][1:79]*
        mort_i$p_surv[mort_i$year==ii][1:79]

      country_TB_LB2$Population[country_TB_LB2$Year==(ii)][81] <-
        country_TB_LB2$Population[country_TB_LB2$Year==(ii-1)][80]*
        mort_i$p_surv[mort_i$year==ii][80] +
        country_TB_LB2$Population[country_TB_LB2$Year==(ii-1)][81]*
        mort_i$p_surv[mort_i$year==ii][81]*(14/15)

      country_TB_LB2$Population[country_TB_LB2$Year==(ii)][82] <-
        country_TB_LB2$Population[country_TB_LB2$Year==(ii-1)][81]*
        mort_i$p_surv[mort_i$year==ii][81]*(1/15) +
        country_TB_LB2$Population[country_TB_LB2$Year==(ii-1)][82]*
        mort_i$p_surv[mort_i$year==ii][82]
    }

    pop_growth_LB <- aggregate(Population~Year,country_TB_LB2,sum)

    # Now split the population data into separate vectors for each age group
    LLB <- split(country_TB_LB2, country_TB_LB2$AgeGrp)

    ### Cost data
    country_cost <- filter(country_cost0, UID == j) # edited NAM Aug-13

    cost_baseline0 <- filter(country_cost, scenario %in% "baseline") %>%
      select(year, scenario, total_und)
    # Pad the top of the dataframe so that it starts in 2019
    dum <- data.frame(cbind(seq(2019, 2026, 1), cost_baseline0$scenario[1:8], rep(0,8)))

    # Extrapolate to 2080 # edited NAM Aug-17
    dum2 <- data.frame(cbind(2051:2080, rep(cost_baseline0$scenario[1],30),
                             cost_baseline0$total_und[24]*
                               pop_growth_LB$Population[33:62]/pop_growth_LB$Population[32]))

    cost_baseline <- rbind(setNames(dum, names(cost_baseline0)), cost_baseline0,
                           setNames(dum2, names(cost_baseline0)))
    
    # Extrapolate to 2080 # edited NAM Nov-1
    country_yld2 <-- array(NA,dim=c(5+27+30,55)) # UID, year, age
    dimnames(country_yld2)[[1]] <- paste0("yr_",2019:2080)
    dimnames(country_yld2)[[2]] <- paste0("ag_",15:69)
    
    ### Start the loop through scenarios
    for (k in 1:length(scenarios)) {

      Runtype_scenario <- scenarios[k]
      
      # Subset YLD data for single scenario # ACP edits Nov 3 2022
      country_yld1 <- country_yld0[dimnames(country_yld0)[[1]]==Runtype_scenario, , ]
      country_yld2[1:5,] <- 0
      country_yld2[6:32,] <- country_yld1
      for(kk in 1:30){
        country_yld2[32+kk,] <- country_yld2[32,]* pop_growth_LB$Population[32+kk]/pop_growth_LB$Population[32]
      }
      
      # Do the same for vaccine scenario
      country_TB_LV <- filter(country_TB, Runtype %in% Runtype_scenario) %>%
        select(Year, AgeGrp, Population)

      country_TB_LV <- country_TB_LV[which(country_TB_LV$Year>=2027),]

      # Extend country_TB_LV to 2080 # edited NAM Aug-17
      country_TB_LV2 <- expand.grid(ord_nam,2027:2100)[,2:1] ## edited ACP Aug-18
      names(country_TB_LV2) <- c("Year","AgeGrp")
      country_TB_LV2$Population <- NA
      # Fill in observed data
      for(ii in unique(country_TB_LV$Year)){
        for(jj in unique(country_TB_LV$AgeGrp)){
          country_TB_LV2[country_TB_LV2$Year==ii & country_TB_LV2$AgeGrp==jj,3] <-
            country_TB_LV[country_TB_LV$Year==ii & country_TB_LV$AgeGrp==jj,3]
        }
      }
      # Fill in new birth data
      country_TB_LV2$Population[country_TB_LV2$AgeGrp==ord_nam[1] & country_TB_LV2$Year>2050] <-
        as.numeric(births_i$births)/1e3

      # Apply mort rates
      for(ii in 2051:2100){ # ii=2053
        country_TB_LV2$Population[country_TB_LV2$Year==(ii)][2:80] <-
          country_TB_LV2$Population[country_TB_LV2$Year==(ii-1)][1:79]*
          mort_i$p_surv[mort_i$year==ii][1:79]

        country_TB_LV2$Population[country_TB_LV2$Year==(ii)][81] <-
          country_TB_LV2$Population[country_TB_LV2$Year==(ii-1)][80]*
          mort_i$p_surv[mort_i$year==ii][80] +
          country_TB_LV2$Population[country_TB_LV2$Year==(ii-1)][81]*
          mort_i$p_surv[mort_i$year==ii][81]*(14/15)

        country_TB_LV2$Population[country_TB_LV2$Year==(ii)][82] <-
          country_TB_LV2$Population[country_TB_LV2$Year==(ii-1)][81]*
          mort_i$p_surv[mort_i$year==ii][81]*(1/15) +
          country_TB_LV2$Population[country_TB_LV2$Year==(ii-1)][82]*
          mort_i$p_surv[mort_i$year==ii][82]
      }

      pop_growth_LV <- aggregate(Population~Year,country_TB_LV2,sum)

      LLV <- split(country_TB_LV2, country_TB_LV2$AgeGrp)

      ############################
      # Extract the cost sequence
      ############################

      cost_vaccine0 <- filter(country_cost, scenario %in% Runtype_scenario) %>%
        select(year, scenario, total_und)
      # Pad the top of the dataframe so that it starts in 2019
      dum <- data.frame(cbind(seq(2019, 2026, 1), cost_vaccine0$scenario[1:8], replicate(8, 0)))

      # Extrapolate to 2080 # edited NAM Aug-17
      dum2 <- data.frame(cbind(2051:2080, rep(cost_vaccine0$scenario[1],30),
                               cost_vaccine0$total_und[24]*
                                 pop_growth_LV$Population[33:62]/pop_growth_LV$Population[32]))

      cost_vaccine <- rbind(setNames(dum, names(cost_vaccine0)), cost_vaccine0,
                             setNames(dum2, names(cost_vaccine0)))

      ###############################################
      # Macro parameters section from datamerge file
      ###############################################
       pwt <- datamerge %>% filter(iso3c == i) %>%
        select(year, iso3c, delta, h, g)
      sdata <- datamerge %>% filter(iso3c == i) %>%
        select(year, iso3c, s)
      alphadata0 <- datamerge %>% filter(iso3c == i) %>%
        select(year, iso3c, alpha)

      ###############################
      # GDP initial calibration data
      ###############################
      wdi0 <- wdi[which(wdi$iso3c==i),]
      wdix <- wdifull[which(wdifull$iso3c==i),]
      if (i=="ERI"|i=="SSD"|i=="SYR"|i=="VEN") {

        gdp2020 <- wdix$GDP[1]
        gdp2019 <- wdix$GDP[2]

      } else {

        gdp2020 <- wdi0$GDP[2]
        gdp2019 <- wdi0$GDP[1]

      }


      ##################################
      # Labor force participation rates
      ##################################
      # Labor force participation rates are done by five year bands
      # Needed to use a backdoor to get the original data which has been
      # replaced in the web interface for ilostat
      i_temp <- i
      if (i=="ERI") {i_temp<-"ETH"}
      if (i=="SSD") {i_temp<-"SDN"}
      # Load Rilostat library
      lfpr0 <- lfpr0full[which(lfpr0full$ref_area==i_temp),]
      # Latest year varies a lot by country
      latestyear <- max(lfpr0$time)
      # Rerun the command picking out only latest year automatically
      lfpr1 <- get_ilostat(
        id = c("EAP_DWAP_SEX_AGE_RT_A"),
        filters = list(
          ref_area = i_temp,
          classif1 = c("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24",
                       "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34",
                       "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44",
                       "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54",
                       "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64",
                       "AGE_5YRBANDS_YGE65"),
          time = latestyear,
          sex = c("T")
        ), quiet = TRUE
      ) %>% select(ref_area, classif1, obs_value)

      # Fill in missing LFPR values
      lfpbands <- data.frame(rbind("AGE_5YRBANDS_Y15-19", "AGE_5YRBANDS_Y20-24",
                                   "AGE_5YRBANDS_Y25-29", "AGE_5YRBANDS_Y30-34",
                                   "AGE_5YRBANDS_Y35-39", "AGE_5YRBANDS_Y40-44",
                                   "AGE_5YRBANDS_Y45-49", "AGE_5YRBANDS_Y50-54",
                                   "AGE_5YRBANDS_Y55-59", "AGE_5YRBANDS_Y60-64",
                                   "AGE_5YRBANDS_YGE65"))
      names(lfpbands) <- c("classif1")
      lfpr <- merge(x=lfpbands, y=lfpr1, by = c("classif1"), all.x=TRUE) %>%
        fill(obs_value, .direction = 'down')
      lfp15_19 <- as.numeric(lfpr$obs_value[1]) / 100
      lfp20_24 <- as.numeric(lfpr$obs_value[2]) / 100
      lfp25_29 <- as.numeric(lfpr$obs_value[3]) / 100
      lfp30_34 <- as.numeric(lfpr$obs_value[4]) / 100
      lfp35_39 <- as.numeric(lfpr$obs_value[5]) / 100
      lfp40_44 <- as.numeric(lfpr$obs_value[6]) / 100
      lfp45_49 <- as.numeric(lfpr$obs_value[7]) / 100
      lfp50_54 <- as.numeric(lfpr$obs_value[8]) / 100
      lfp55_59 <- as.numeric(lfpr$obs_value[9]) / 100
      lfp60_64 <- as.numeric(lfpr$obs_value[10]) / 100
      lfpGE65 <- as.numeric(lfpr$obs_value[11]) / 100

      ##########################################################
      # Cuddington experience skill augmentation
      # We should be able to do much better than this
      # using some results from Mincerian wage regressions,
      # but which might be country specific
      # See Cuddington, John T. and John D. Hancock,
      # "Assessing the Impact of AIDS on the Growth Path of
      # the Malawian Economy," Journal of Development Economics,
      # 1994, 43(2), 363-368.
      ##########################################################
      rho1 <- 0.8
      rho2 <- 0.02
      rho3 <- -0.0002
      # Create the Cuddington coefs for each age category between 15 and 69
      # can be modified as desired
      ll0 <- seq(15, 69, by = 1)
      rho <- function(x) (rho1 + rho2 * (x - 15) + rho3 * (x - 15)^2)
      cud <- lapply(ll0, rho)
      names(cud) <- paste0("rho", 15:69)
      # Format of each Cuddington coefficient
      # will be "cud$rhoXX" for XX year olds


      ##################
      # Simulation code
      ##################

      # Turn the whole simulation code into a single function

      cipe <- function(alpha_vals,  # edited NAM Aug-11
                       g_vals,  # edited NAM Aug-11
                       s_vals,  # edited NAM Aug-11
                       delta_vals,  # edited NAM Aug-11
                       h_vals) {  # edited NAM Aug-11

        # Stochastic coefficients
        # Coefficient associated with capital in the production function
        # Note: do NOT set seed because we are drawing a single random number
        alpha_t <- sample(alpha_vals,100,replace=TRUE)  # edited NAM Aug-11

        # Growth rate of TFP
        # Note that if g is 0, then GDP per efficiency unit of labor will
        # eventually converge to a constant SS level. If g > 0 GDP per
        # efficiency unit of labor will converge to exactly that growth rate
        g_t <- sample(g_vals,100,replace=TRUE)  # edited NAM Aug-11

        # Saving/Investment rate
        s_t <- sample(s_vals,100,replace=TRUE)  # edited NAM Aug-11

        # Depreciation rate
        delta_t <- sample(delta_vals,100,replace=TRUE)  # edited NAM Aug-11

        # Growth rate of human capital
        h_t <- sample(h_vals,100,replace=TRUE)  # edited NAM Aug-11

        ###############################################################
        # Set up the parameters
        # Several more of these will subsequently be estimated from data
        ###############################################################
        # Desired time frame
        # Start date in data 2019.5
        # End date in data 2050.5
        T <- 32 + 30 # edited NAM Aug-17 for extrap to 2080

        # The following are complete guesses both here and in the existing EPIC model

        # Proportion of cost of treatment funded from savings
        r  <- 1
        # Productivity losses due to morbidity in 0-4 and 5-14 age groups
        q1 <- 1
        q2 <- 0.5

        ##################################
        # Setup of vectors for simulation
        ##################################

        # Variables that are endogenous to the model
        A    <- rep(NA, T)
        A_B  <- rep(NA, T)
        K    <- rep(NA, T)
        K_B  <- rep(NA, T)
        H    <- rep(NA, T)
        H_B  <- rep(NA, T)
        La   <- rep(NA, T)
        La_B <- rep(NA, T)
        L    <- rep(NA, T)
        L_B  <- rep(NA, T)
        Y    <- rep(NA, T)
        Y_B  <- rep(NA, T)
        y    <- rep(NA, T)
        y_B  <- rep(NA, T)
        test <- rep(NA, T)
        year <- rep(NA, T)
        gain <- rep(NA, T)
        percentagegain <- rep(NA, T)

        WP   <- rep(NA, T)
        WP_B <- rep(NA, T) # edited ACP 23-Aug
        TotPop <- rep(NA, T) # edited ACP 23-Aug
        TotPop_B <- rep(NA, T) # edited ACP 23-Aug

        Y0   <- rep(NA, T)
        Y0_B <- rep(NA, T)

        # Population projection data
        LB0  <- as.numeric(unlist(LLB$"[0,0]"[, 3]))
        LB1  <- as.numeric(unlist(LLB$"(1,1]"[, 3]))
        LB2  <- as.numeric(unlist(LLB$"(2,2]"[, 3]))
        LB3  <- as.numeric(unlist(LLB$"(3,3]"[, 3]))
        LB4  <- as.numeric(unlist(LLB$"(4,4]"[, 3]))
        LB5  <- as.numeric(unlist(LLB$"(5,5]"[, 3]))
        LB6  <- as.numeric(unlist(LLB$"(6,6]"[, 3]))
        LB7  <- as.numeric(unlist(LLB$"(7,7]"[, 3]))
        LB8  <- as.numeric(unlist(LLB$"(8,8]"[, 3]))
        LB9  <- as.numeric(unlist(LLB$"(9,9]"[, 3]))
        LB10 <- as.numeric(unlist(LLB$"(10,10]"[, 3]))
        LB11 <- as.numeric(unlist(LLB$"(11,11]"[, 3]))
        LB12 <- as.numeric(unlist(LLB$"(12,12]"[, 3]))
        LB13 <- as.numeric(unlist(LLB$"(13,13]"[, 3]))
        LB14 <- as.numeric(unlist(LLB$"(14,14]"[, 3]))
        LB15 <- as.numeric(unlist(LLB$"(15,15]"[, 3]))
        LB16 <- as.numeric(unlist(LLB$"(16,16]"[, 3]))
        LB17 <- as.numeric(unlist(LLB$"(17,17]"[, 3]))
        LB18 <- as.numeric(unlist(LLB$"(18,18]"[, 3]))
        LB19 <- as.numeric(unlist(LLB$"(19,19]"[, 3]))
        LB20 <- as.numeric(unlist(LLB$"(20,20]"[, 3]))
        LB21 <- as.numeric(unlist(LLB$"(21,21]"[, 3]))
        LB22 <- as.numeric(unlist(LLB$"(22,22]"[, 3]))
        LB23 <- as.numeric(unlist(LLB$"(23,23]"[, 3]))
        LB24 <- as.numeric(unlist(LLB$"(24,24]"[, 3]))
        LB25 <- as.numeric(unlist(LLB$"(25,25]"[, 3]))
        LB26 <- as.numeric(unlist(LLB$"(26,26]"[, 3]))
        LB27 <- as.numeric(unlist(LLB$"(27,27]"[, 3]))
        LB28 <- as.numeric(unlist(LLB$"(28,28]"[, 3]))
        LB29 <- as.numeric(unlist(LLB$"(29,29]"[, 3]))
        LB30 <- as.numeric(unlist(LLB$"(30,30]"[, 3]))
        LB31 <- as.numeric(unlist(LLB$"(31,31]"[, 3]))
        LB32 <- as.numeric(unlist(LLB$"(32,32]"[, 3]))
        LB33 <- as.numeric(unlist(LLB$"(33,33]"[, 3]))
        LB34 <- as.numeric(unlist(LLB$"(34,34]"[, 3]))
        LB35 <- as.numeric(unlist(LLB$"(35,35]"[, 3]))
        LB36 <- as.numeric(unlist(LLB$"(36,36]"[, 3]))
        LB37 <- as.numeric(unlist(LLB$"(37,37]"[, 3]))
        LB38 <- as.numeric(unlist(LLB$"(38,38]"[, 3]))
        LB39 <- as.numeric(unlist(LLB$"(39,39]"[, 3]))
        LB40 <- as.numeric(unlist(LLB$"(40,40]"[, 3]))
        LB41 <- as.numeric(unlist(LLB$"(41,41]"[, 3]))
        LB42 <- as.numeric(unlist(LLB$"(42,42]"[, 3]))
        LB43 <- as.numeric(unlist(LLB$"(43,43]"[, 3]))
        LB44 <- as.numeric(unlist(LLB$"(44,44]"[, 3]))
        LB45 <- as.numeric(unlist(LLB$"(45,45]"[, 3]))
        LB46 <- as.numeric(unlist(LLB$"(46,46]"[, 3]))
        LB47 <- as.numeric(unlist(LLB$"(47,47]"[, 3]))
        LB48 <- as.numeric(unlist(LLB$"(48,48]"[, 3]))
        LB49 <- as.numeric(unlist(LLB$"(49,49]"[, 3]))
        LB50 <- as.numeric(unlist(LLB$"(50,50]"[, 3]))
        LB51 <- as.numeric(unlist(LLB$"(51,51]"[, 3]))
        LB52 <- as.numeric(unlist(LLB$"(52,52]"[, 3]))
        LB53 <- as.numeric(unlist(LLB$"(53,53]"[, 3]))
        LB54 <- as.numeric(unlist(LLB$"(54,54]"[, 3]))
        LB55 <- as.numeric(unlist(LLB$"(55,55]"[, 3]))
        LB56 <- as.numeric(unlist(LLB$"(56,56]"[, 3]))
        LB57 <- as.numeric(unlist(LLB$"(57,57]"[, 3]))
        LB58 <- as.numeric(unlist(LLB$"(58,58]"[, 3]))
        LB59 <- as.numeric(unlist(LLB$"(59,59]"[, 3]))
        LB60 <- as.numeric(unlist(LLB$"(60,60]"[, 3]))
        LB61 <- as.numeric(unlist(LLB$"(61,61]"[, 3]))
        LB62 <- as.numeric(unlist(LLB$"(62,62]"[, 3]))
        LB63 <- as.numeric(unlist(LLB$"(63,63]"[, 3]))
        LB64 <- as.numeric(unlist(LLB$"(64,64]"[, 3]))
        LB65 <- as.numeric(unlist(LLB$"(65,65]"[, 3]))
        LB66 <- as.numeric(unlist(LLB$"(66,66]"[, 3]))
        LB67 <- as.numeric(unlist(LLB$"(67,67]"[, 3]))
        LB68 <- as.numeric(unlist(LLB$"(68,68]"[, 3]))
        LB69 <- as.numeric(unlist(LLB$"(69,69]"[, 3]))
        LB70 <- as.numeric(unlist(LLB$"(70,70]"[, 3]))
        LB71 <- as.numeric(unlist(LLB$"(71,71]"[, 3]))
        LB72 <- as.numeric(unlist(LLB$"(72,72]"[, 3]))
        LB73 <- as.numeric(unlist(LLB$"(73,73]"[, 3]))
        LB74 <- as.numeric(unlist(LLB$"(74,74]"[, 3]))
        LB75 <- as.numeric(unlist(LLB$"(75,75]"[, 3]))
        LB76 <- as.numeric(unlist(LLB$"(76,76]"[, 3]))
        LB77 <- as.numeric(unlist(LLB$"(77,77]"[, 3]))
        LB78 <- as.numeric(unlist(LLB$"(78,78]"[, 3]))
        LB79 <- as.numeric(unlist(LLB$"(79,79]"[, 3]))
        LB80 <- as.numeric(unlist(LLB$"(80,89]"[, 3]))
        LB90 <- as.numeric(unlist(LLB$"(90,99]"[, 3]))

        ##########################################
        # Initialize intervention, averted deaths
        # and averted morbidity sequences
        ##########################################

        # Cost of intervention
        # In baseline
        C_B <- rep(0, T)
        # In intervention
        C   <- rep(0, T)

        # Averted deaths by age category
        z0  <- rep(0, T)
        z1  <- rep(0, T)
        z2  <- rep(0, T)
        z3  <- rep(0, T)
        z4  <- rep(0, T)
        z5  <- rep(0, T)
        z6  <- rep(0, T)
        z7  <- rep(0, T)
        z8  <- rep(0, T)
        z9  <- rep(0, T)
        z10 <- rep(0, T)
        z11 <- rep(0, T)
        z12 <- rep(0, T)
        z13 <- rep(0, T)
        z14 <- rep(0, T)
        z15 <- rep(0, T)
        z16 <- rep(0, T)
        z17 <- rep(0, T)
        z18 <- rep(0, T)
        z19 <- rep(0, T)
        z20 <- rep(0, T)
        z21 <- rep(0, T)
        z22 <- rep(0, T)
        z23 <- rep(0, T)
        z24 <- rep(0, T)
        z25 <- rep(0, T)
        z26 <- rep(0, T)
        z27 <- rep(0, T)
        z28 <- rep(0, T)
        z29 <- rep(0, T)
        z30 <- rep(0, T)
        z31 <- rep(0, T)
        z32 <- rep(0, T)
        z33 <- rep(0, T)
        z34 <- rep(0, T)
        z35 <- rep(0, T)
        z36 <- rep(0, T)
        z37 <- rep(0, T)
        z38 <- rep(0, T)
        z39 <- rep(0, T)
        z40 <- rep(0, T)
        z41 <- rep(0, T)
        z42 <- rep(0, T)
        z43 <- rep(0, T)
        z44 <- rep(0, T)
        z45 <- rep(0, T)
        z46 <- rep(0, T)
        z47 <- rep(0, T)
        z48 <- rep(0, T)
        z49 <- rep(0, T)
        z50 <- rep(0, T)
        z51 <- rep(0, T)
        z52 <- rep(0, T)
        z53 <- rep(0, T)
        z54 <- rep(0, T)
        z55 <- rep(0, T)
        z56 <- rep(0, T)
        z57 <- rep(0, T)
        z58 <- rep(0, T)
        z59 <- rep(0, T)
        z60 <- rep(0, T)
        z61 <- rep(0, T)
        z62 <- rep(0, T)
        z63 <- rep(0, T)
        z64 <- rep(0, T)
        z65 <- rep(0, T)
        z66 <- rep(0, T)
        z67 <- rep(0, T)
        z68 <- rep(0, T)
        z69 <- rep(0, T)
        z70 <- rep(0, T)
        z71 <- rep(0, T)
        z72 <- rep(0, T)
        z73 <- rep(0, T)
        z74 <- rep(0, T)
        z75 <- rep(0, T)
        z76 <- rep(0, T)
        z77 <- rep(0, T)
        z78 <- rep(0, T)
        z79 <- rep(0, T)
        z80 <- rep(0, T)
        z90 <- rep(0, T)

        # Averted productivity loss by age category
        b0  <- rep(0, T)
        b1  <- rep(0, T)
        b2  <- rep(0, T)
        b3  <- rep(0, T)
        b4  <- rep(0, T)
        b5  <- rep(0, T)
        b6  <- rep(0, T)
        b7  <- rep(0, T)
        b8  <- rep(0, T)
        b9  <- rep(0, T)
        b10 <- rep(0, T)
        b11 <- rep(0, T)
        b12 <- rep(0, T)
        b13 <- rep(0, T)
        b14 <- rep(0, T)
        b15 <- rep(0, T)
        b16 <- rep(0, T)
        b17 <- rep(0, T)
        b18 <- rep(0, T)
        b19 <- rep(0, T)
        b20 <- rep(0, T)
        b21 <- rep(0, T)
        b22 <- rep(0, T)
        b23 <- rep(0, T)
        b24 <- rep(0, T)
        b25 <- rep(0, T)
        b26 <- rep(0, T)
        b27 <- rep(0, T)
        b28 <- rep(0, T)
        b29 <- rep(0, T)
        b30 <- rep(0, T)
        b31 <- rep(0, T)
        b32 <- rep(0, T)
        b33 <- rep(0, T)
        b34 <- rep(0, T)
        b35 <- rep(0, T)
        b36 <- rep(0, T)
        b37 <- rep(0, T)
        b38 <- rep(0, T)
        b39 <- rep(0, T)
        b40 <- rep(0, T)
        b41 <- rep(0, T)
        b42 <- rep(0, T)
        b43 <- rep(0, T)
        b44 <- rep(0, T)
        b45 <- rep(0, T)
        b46 <- rep(0, T)
        b47 <- rep(0, T)
        b48 <- rep(0, T)
        b49 <- rep(0, T)
        b50 <- rep(0, T)
        b51 <- rep(0, T)
        b52 <- rep(0, T)
        b53 <- rep(0, T)
        b54 <- rep(0, T)
        b55 <- rep(0, T)
        b56 <- rep(0, T)
        b57 <- rep(0, T)
        b58 <- rep(0, T)
        b59 <- rep(0, T)
        b60 <- rep(0, T)
        b61 <- rep(0, T)
        b62 <- rep(0, T)
        b63 <- rep(0, T)
        b64 <- rep(0, T)
        b65 <- rep(0, T)
        b66 <- rep(0, T)
        b67 <- rep(0, T)
        b68 <- rep(0, T)
        b69 <- rep(0, T)
        b70 <- rep(0, T)
        b71 <- rep(0, T)
        b72 <- rep(0, T)
        b73 <- rep(0, T)
        b74 <- rep(0, T)
        b75 <- rep(0, T)
        b76 <- rep(0, T)
        b77 <- rep(0, T)
        b78 <- rep(0, T)
        b79 <- rep(0, T)
        b80 <- rep(0, T)
        b90 <- rep(0, T)

        # The specific numbers for given time periods
        # will be entered inside the simulation model
        # in the "scenario" section

        #################################################################
        # Calibration section
        #################################################################
        year[1] <- 1
        A[1]    <- 1
        A_B[1]  <- 1
        # Approximate steady state capital stock calibration from theoretical Solow model
        K[1]    <- mean(s_vals)*(gdp2019 / 1000000)/(mean(g_vals) + mean(delta_vals))  # edited NAM Aug-11, then Aug-13
        K_B[1]  <- K[1]  # edited NAM Aug-11
        H[1]    <- 1
        H_B[1]  <- 1
        La[1]   <- 1
        La_B[1] <- 1
        L[1]    <- 1
        L_B[1]  <- 1
        # Need to start with true value of GDP in 2019 because of lagged
        # structure of the national income accounting identity
        # and subsequently add multiplicative factor to calibrate to 2020
        Y[1]    <- gdp2019 / 1000000
        Y_B[1]  <- gdp2019 / 1000000
        Y0[1]   <- gdp2019 / 1000000
        Y0_B[1] <- gdp2019 / 1000000
        C[1]    <- 0
        C_B[1]  <- 0
        gain[1] <- 0
        percentagegain[1] <- 0

        ############################################
        # Simulation model
        # Note: the order in which equations appear
        # in the function is extremely important
        ############################################

        C_B <- as.numeric(cost_baseline$total_und) / 1000

        # Baseline case ("_B" suffix)
        for (t in 2:T) {
          L_B[t] <-
            0 * (LB0[t]) +
            0 * (LB1[t]) +
            0 * (LB2[t]) +
            0 * (LB3[t]) +
            0 * (LB4[t]) +
            0 * (LB5[t]) +
            0 * (LB6[t]) +
            0 * (LB7[t]) +
            0 * (LB8[t]) +
            0 * (LB9[t]) +
            0 * (LB10[t]) +
            0 * (LB11[t]) +
            0 * (LB12[t]) +
            0 * (LB13[t]) +
            0 * (LB14[t]) +
            lfp15_19 * cud$rho15 * LB15[t] +
            lfp15_19 * cud$rho16 * LB16[t] +
            lfp15_19 * cud$rho17 * LB17[t] +
            lfp15_19 * cud$rho18 * LB18[t] +
            lfp15_19 * cud$rho19 * LB19[t] +
            lfp20_24 * cud$rho20 * LB20[t] +
            lfp20_24 * cud$rho21 * LB21[t] +
            lfp20_24 * cud$rho22 * LB22[t] +
            lfp20_24 * cud$rho23 * LB23[t] +
            lfp20_24 * cud$rho24 * LB24[t] +
            lfp25_29 * cud$rho25 * LB25[t] +
            lfp25_29 * cud$rho26 * LB26[t] +
            lfp25_29 * cud$rho27 * LB27[t] +
            lfp25_29 * cud$rho28 * LB28[t] +
            lfp25_29 * cud$rho29 * LB29[t] +
            lfp30_34 * cud$rho30 * LB30[t] +
            lfp30_34 * cud$rho31 * LB31[t] +
            lfp30_34 * cud$rho32 * LB32[t] +
            lfp30_34 * cud$rho33 * LB33[t] +
            lfp30_34 * cud$rho34 * LB34[t] +
            lfp35_39 * cud$rho35 * LB35[t] +
            lfp35_39 * cud$rho36 * LB36[t] +
            lfp35_39 * cud$rho37 * LB37[t] +
            lfp35_39 * cud$rho38 * LB38[t] +
            lfp35_39 * cud$rho39 * LB39[t] +
            lfp40_44 * cud$rho40 * LB40[t] +
            lfp40_44 * cud$rho41 * LB41[t] +
            lfp40_44 * cud$rho42 * LB42[t] +
            lfp40_44 * cud$rho43 * LB43[t] +
            lfp40_44 * cud$rho44 * LB44[t] +
            lfp45_49 * cud$rho45 * LB45[t] +
            lfp45_49 * cud$rho46 * LB46[t] +
            lfp45_49 * cud$rho47 * LB47[t] +
            lfp45_49 * cud$rho48 * LB48[t] +
            lfp45_49 * cud$rho49 * LB49[t] +
            lfp50_54 * cud$rho50 * LB50[t] +
            lfp50_54 * cud$rho51 * LB51[t] +
            lfp50_54 * cud$rho52 * LB52[t] +
            lfp50_54 * cud$rho53 * LB53[t] +
            lfp50_54 * cud$rho54 * LB54[t] +
            lfp55_59 * cud$rho55 * LB55[t] +
            lfp55_59 * cud$rho56 * LB56[t] +
            lfp55_59 * cud$rho57 * LB57[t] +
            lfp55_59 * cud$rho58 * LB58[t] +
            lfp55_59 * cud$rho59 * LB59[t] +
            lfp60_64 * cud$rho60 * LB60[t] +
            lfp60_64 * cud$rho61 * LB61[t] +
            lfp60_64 * cud$rho62 * LB62[t] +
            lfp60_64 * cud$rho63 * LB63[t] +
            lfp60_64 * cud$rho64 * LB64[t] +
            lfpGE65 * cud$rho65 * LB65[t] +
            lfpGE65 * cud$rho66 * LB66[t] +
            lfpGE65 * cud$rho67 * LB67[t] +
            lfpGE65 * cud$rho68 * LB68[t] +
            lfpGE65 * cud$rho69 * LB69[t] +
            0 * LB70[t] +
            0 * LB71[t] +
            0 * LB72[t] +
            0 * LB73[t] +
            0 * LB74[t] +
            0 * LB75[t] +
            0 * LB76[t] +
            0 * LB77[t] +
            0 * LB78[t] +
            0 * LB79[t] +
            0 * LB80[t] +
            0 * LB90[t]
          A_B[t]  <- A_B[1] * exp(sum(g_t[1:t]))  # edited NAM Aug-11
          H_B[t]  <- H_B[1] * exp(sum(h_t[1:t]))  # edited NAM Aug-11
          K_B[t]  <- s_t[t] * Y_B[t - 1] - r * C_B[t - 1] + (1 - delta_t[t]) * K_B[t - 1]  # edited NAM Aug-11
          # This is the nuisance parameter for scaling
          Y0_B[t] <- A_B[t] * (K_B[t]^alpha_t[t]) * ((H_B[t] * L_B[t])^(1 - alpha_t[t]))  # edited NAM Aug-11
          NP      <- gdp2020 / (Y0_B[2] * 1000000)
          Y_B[t]  <- NP * A_B[t] * (K_B[t]^alpha_t[t]) * ((H_B[t] * L_B[t])^(1 - alpha_t[t]))  # edited NAM Aug-11
          y_B[t]  <- Y_B[t] / L_B[t]

          ## ADDITIONAL OUTCOME = BASELINE WORKING AGE POP # edited ACP 25-Aug
          WP_B[t] <-
            LB15[t] +
            LB16[t] +
            LB17[t] +
            LB18[t] +
            LB19[t] +
            LB20[t] +
            LB21[t] +
            LB22[t] +
            LB23[t] +
            LB24[t] +
            LB25[t] +
            LB26[t] +
            LB27[t] +
            LB28[t] +
            LB29[t] +
            LB30[t] +
            LB31[t] +
            LB32[t] +
            LB33[t] +
            LB34[t] +
            LB35[t] +
            LB36[t] +
            LB37[t] +
            LB38[t] +
            LB39[t] +
            LB40[t] +
            LB41[t] +
            LB42[t] +
            LB43[t] +
            LB44[t] +
            LB45[t] +
            LB46[t] +
            LB47[t] +
            LB48[t] +
            LB49[t] +
            LB50[t] +
            LB51[t] +
            LB52[t] +
            LB53[t] +
            LB54[t] +
            LB55[t] +
            LB56[t] +
            LB57[t] +
            LB58[t] +
            LB59[t] +
            LB60[t] +
            LB61[t] +
            LB62[t] +
            LB63[t] +
            LB64[t] +
            LB65[t] +
            LB66[t] +
            LB67[t] +
            LB68[t] +
            LB69[t]

          ## ADDITIONAL OUTCOME = BASELINE TOTAL POP # edited ACP 25-Aug
          TotPop_B[t] <-
            (LB0[t]) +
            (LB1[t]) +
            (LB2[t]) +
            (LB3[t]) +
            (LB4[t]) +
            (LB5[t]) +
            (LB6[t]) +
            (LB7[t]) +
            (LB8[t]) +
            (LB9[t]) +
            (LB10[t]) +
            (LB11[t]) +
            (LB12[t]) +
            (LB13[t]) +
            (LB14[t]) +
            LB15[t] +
            LB16[t] +
            LB17[t] +
            LB18[t] +
            LB19[t] +
            LB20[t] +
            LB21[t] +
            LB22[t] +
            LB23[t] +
            LB24[t] +
            LB25[t] +
            LB26[t] +
            LB27[t] +
            LB28[t] +
            LB29[t] +
            LB30[t] +
            LB31[t] +
            LB32[t] +
            LB33[t] +
            LB34[t] +
            LB35[t] +
            LB36[t] +
            LB37[t] +
            LB38[t] +
            LB39[t] +
            LB40[t] +
            LB41[t] +
            LB42[t] +
            LB43[t] +
            LB44[t] +
            LB45[t] +
            LB46[t] +
            LB47[t] +
            LB48[t] +
            LB49[t] +
            LB50[t] +
            LB51[t] +
            LB52[t] +
            LB53[t] +
            LB54[t] +
            LB55[t] +
            LB56[t] +
            LB57[t] +
            LB58[t] +
            LB59[t] +
            LB60[t] +
            LB61[t] +
            LB62[t] +
            LB63[t] +
            LB64[t] +
            LB65[t] +
            LB66[t] +
            LB67[t] +
            LB68[t] +
            LB69[t] +
            LB70[t] +
            LB71[t] +
            LB72[t] +
            LB73[t] +
            LB74[t] +
            LB75[t] +
            LB76[t] +
            LB77[t] +
            LB78[t] +
            LB79[t] +
            LB80[t] +
            LB90[t]

        }

        # Scenario

        C <- as.numeric(cost_vaccine$total_und) / 1000

        # Population projection data under vaccine scenario
        # Since the dimension in the vaccine runs are NOT the same
        # need to concatenate the first 8 observations from the baseline!
        LV0  <- c(LB0[1:8], as.numeric(unlist(LLV$"[0,0]"[, 3])))
        LV1  <- c(LB1[1:8], as.numeric(unlist(LLV$"(1,1]"[, 3])))
        LV2  <- c(LB2[1:8], as.numeric(unlist(LLV$"(2,2]"[, 3])))
        LV3  <- c(LB3[1:8], as.numeric(unlist(LLV$"(3,3]"[, 3])))
        LV4  <- c(LB4[1:8], as.numeric(unlist(LLV$"(4,4]"[, 3])))
        LV5  <- c(LB5[1:8], as.numeric(unlist(LLV$"(5,5]"[, 3])))
        LV6  <- c(LB6[1:8], as.numeric(unlist(LLV$"(6,6]"[, 3])))
        LV7  <- c(LB7[1:8], as.numeric(unlist(LLV$"(7,7]"[, 3])))
        LV8  <- c(LB8[1:8], as.numeric(unlist(LLV$"(8,8]"[, 3])))
        LV9  <- c(LB9[1:8], as.numeric(unlist(LLV$"(9,9]"[, 3])))
        LV10 <- c(LB10[1:8], as.numeric(unlist(LLV$"(10,10]"[, 3])))
        LV11 <- c(LB11[1:8], as.numeric(unlist(LLV$"(11,11]"[, 3])))
        LV12 <- c(LB12[1:8], as.numeric(unlist(LLV$"(12,12]"[, 3])))
        LV13 <- c(LB13[1:8], as.numeric(unlist(LLV$"(13,13]"[, 3])))
        LV14 <- c(LB14[1:8], as.numeric(unlist(LLV$"(14,14]"[, 3])))
        LV15 <- c(LB15[1:8], as.numeric(unlist(LLV$"(15,15]"[, 3])))
        LV16 <- c(LB16[1:8], as.numeric(unlist(LLV$"(16,16]"[, 3])))
        LV17 <- c(LB17[1:8], as.numeric(unlist(LLV$"(17,17]"[, 3])))
        LV18 <- c(LB18[1:8], as.numeric(unlist(LLV$"(18,18]"[, 3])))
        LV19 <- c(LB19[1:8], as.numeric(unlist(LLV$"(19,19]"[, 3])))
        LV20 <- c(LB20[1:8], as.numeric(unlist(LLV$"(20,20]"[, 3])))
        LV21 <- c(LB21[1:8], as.numeric(unlist(LLV$"(21,21]"[, 3])))
        LV22 <- c(LB22[1:8], as.numeric(unlist(LLV$"(22,22]"[, 3])))
        LV23 <- c(LB23[1:8], as.numeric(unlist(LLV$"(23,23]"[, 3])))
        LV24 <- c(LB24[1:8], as.numeric(unlist(LLV$"(24,24]"[, 3])))
        LV25 <- c(LB25[1:8], as.numeric(unlist(LLV$"(25,25]"[, 3])))
        LV26 <- c(LB26[1:8], as.numeric(unlist(LLV$"(26,26]"[, 3])))
        LV27 <- c(LB27[1:8], as.numeric(unlist(LLV$"(27,27]"[, 3])))
        LV28 <- c(LB28[1:8], as.numeric(unlist(LLV$"(28,28]"[, 3])))
        LV29 <- c(LB29[1:8], as.numeric(unlist(LLV$"(29,29]"[, 3])))
        LV30 <- c(LB30[1:8], as.numeric(unlist(LLV$"(30,30]"[, 3])))
        LV31 <- c(LB31[1:8], as.numeric(unlist(LLV$"(31,31]"[, 3])))
        LV32 <- c(LB32[1:8], as.numeric(unlist(LLV$"(32,32]"[, 3])))
        LV33 <- c(LB33[1:8], as.numeric(unlist(LLV$"(33,33]"[, 3])))
        LV34 <- c(LB34[1:8], as.numeric(unlist(LLV$"(34,34]"[, 3])))
        LV35 <- c(LB35[1:8], as.numeric(unlist(LLV$"(35,35]"[, 3])))
        LV36 <- c(LB36[1:8], as.numeric(unlist(LLV$"(36,36]"[, 3])))
        LV37 <- c(LB37[1:8], as.numeric(unlist(LLV$"(37,37]"[, 3])))
        LV38 <- c(LB38[1:8], as.numeric(unlist(LLV$"(38,38]"[, 3])))
        LV39 <- c(LB39[1:8], as.numeric(unlist(LLV$"(39,39]"[, 3])))
        LV40 <- c(LB40[1:8], as.numeric(unlist(LLV$"(40,40]"[, 3])))
        LV41 <- c(LB41[1:8], as.numeric(unlist(LLV$"(41,41]"[, 3])))
        LV42 <- c(LB42[1:8], as.numeric(unlist(LLV$"(42,42]"[, 3])))
        LV43 <- c(LB43[1:8], as.numeric(unlist(LLV$"(43,43]"[, 3])))
        LV44 <- c(LB44[1:8], as.numeric(unlist(LLV$"(44,44]"[, 3])))
        LV45 <- c(LB45[1:8], as.numeric(unlist(LLV$"(45,45]"[, 3])))
        LV46 <- c(LB46[1:8], as.numeric(unlist(LLV$"(46,46]"[, 3])))
        LV47 <- c(LB47[1:8], as.numeric(unlist(LLV$"(47,47]"[, 3])))
        LV48 <- c(LB48[1:8], as.numeric(unlist(LLV$"(48,48]"[, 3])))
        LV49 <- c(LB49[1:8], as.numeric(unlist(LLV$"(49,49]"[, 3])))
        LV50 <- c(LB50[1:8], as.numeric(unlist(LLV$"(50,50]"[, 3])))
        LV51 <- c(LB51[1:8], as.numeric(unlist(LLV$"(51,51]"[, 3])))
        LV52 <- c(LB52[1:8], as.numeric(unlist(LLV$"(52,52]"[, 3])))
        LV53 <- c(LB53[1:8], as.numeric(unlist(LLV$"(53,53]"[, 3])))
        LV54 <- c(LB54[1:8], as.numeric(unlist(LLV$"(54,54]"[, 3])))
        LV55 <- c(LB55[1:8], as.numeric(unlist(LLV$"(55,55]"[, 3])))
        LV56 <- c(LB56[1:8], as.numeric(unlist(LLV$"(56,56]"[, 3])))
        LV57 <- c(LB57[1:8], as.numeric(unlist(LLV$"(57,57]"[, 3])))
        LV58 <- c(LB58[1:8], as.numeric(unlist(LLV$"(58,58]"[, 3])))
        LV59 <- c(LB59[1:8], as.numeric(unlist(LLV$"(59,59]"[, 3])))
        LV60 <- c(LB60[1:8], as.numeric(unlist(LLV$"(60,60]"[, 3])))
        LV61 <- c(LB61[1:8], as.numeric(unlist(LLV$"(61,61]"[, 3])))
        LV62 <- c(LB62[1:8], as.numeric(unlist(LLV$"(62,62]"[, 3])))
        LV63 <- c(LB63[1:8], as.numeric(unlist(LLV$"(63,63]"[, 3])))
        LV64 <- c(LB64[1:8], as.numeric(unlist(LLV$"(64,64]"[, 3])))
        LV65 <- c(LB65[1:8], as.numeric(unlist(LLV$"(65,65]"[, 3])))
        LV66 <- c(LB66[1:8], as.numeric(unlist(LLV$"(66,66]"[, 3])))
        LV67 <- c(LB67[1:8], as.numeric(unlist(LLV$"(67,67]"[, 3])))
        LV68 <- c(LB68[1:8], as.numeric(unlist(LLV$"(68,68]"[, 3])))
        LV69 <- c(LB69[1:8], as.numeric(unlist(LLV$"(69,69]"[, 3])))
        LV70 <- c(LB70[1:8], as.numeric(unlist(LLV$"(70,70]"[, 3])))
        LV71 <- c(LB71[1:8], as.numeric(unlist(LLV$"(71,71]"[, 3])))
        LV72 <- c(LB72[1:8], as.numeric(unlist(LLV$"(72,72]"[, 3])))
        LV73 <- c(LB73[1:8], as.numeric(unlist(LLV$"(73,73]"[, 3])))
        LV74 <- c(LB74[1:8], as.numeric(unlist(LLV$"(74,74]"[, 3])))
        LV75 <- c(LB75[1:8], as.numeric(unlist(LLV$"(75,75]"[, 3])))
        LV76 <- c(LB76[1:8], as.numeric(unlist(LLV$"(76,76]"[, 3])))
        LV77 <- c(LB77[1:8], as.numeric(unlist(LLV$"(77,77]"[, 3])))
        LV78 <- c(LB78[1:8], as.numeric(unlist(LLV$"(78,78]"[, 3])))
        LV79 <- c(LB79[1:8], as.numeric(unlist(LLV$"(79,79]"[, 3])))
        LV80 <- c(LB80[1:8], as.numeric(unlist(LLV$"(80,89]"[, 3])))
        LV90 <- c(LB90[1:8], as.numeric(unlist(LLV$"(90,99]"[, 3])))

        for (t in 2:T) {
          L[t] <-
            0 * (LV0[t] + z0[t] + b0[t]) +
            0 * (LV1[t] + z1[t] + b1[t]) +
            0 * (LV2[t] + z2[t] + b2[t]) +
            0 * (LV3[t] + z3[t] + b3[t]) +
            0 * (LV4[t] + z4[t] + b4[t]) +
            0 * (LV5[t] + z5[t] + b5[t]) +
            0 * (LV6[t] + z6[t] + b6[t]) +
            0 * (LV7[t] + z7[t] + b7[t]) +
            0 * (LV8[t] + z8[t] + b8[t]) +
            0 * (LV9[t] + z9[t] + b9[t]) +
            0 * (LV10[t] + z10[t] + b10[t]) +
            0 * (LV11[t] + z11[t] + b11[t]) +
            0 * (LV12[t] + z12[t] + b12[t]) +
            0 * (LV13[t] + z13[t] + b13[t]) +
            0 * (LV14[t] + z14[t] + b14[t]) +
            
            lfp15_19 * cud$rho15 * (LV15[t] + z15[t] + b15[t] - country_yld2[t, 1 ]) + # NAM edits Nov 1 2022. 
            lfp15_19 * cud$rho16 * (LV16[t] + z16[t] + b16[t] - country_yld2[t, 2 ]) + # Note this is a subtraction since inc ylds are negative
            lfp15_19 * cud$rho17 * (LV17[t] + z17[t] + b17[t] - country_yld2[t, 3 ]) + # Also note that this assumes t==1 is 2019
            lfp15_19 * cud$rho18 * (LV18[t] + z18[t] + b18[t] - country_yld2[t, 4 ]) + # This holds inc_ylds fixed at their 2050 values for 2051-2080
            lfp15_19 * cud$rho19 * (LV19[t] + z19[t] + b19[t] - country_yld2[t, 5 ]) +
            lfp20_24 * cud$rho20 * (LV20[t] + z20[t] + b20[t] - country_yld2[t, 6 ]) +
            lfp20_24 * cud$rho21 * (LV21[t] + z21[t] + b21[t] - country_yld2[t, 7 ]) +
            lfp20_24 * cud$rho22 * (LV22[t] + z22[t] + b22[t] - country_yld2[t, 8 ]) +
            lfp20_24 * cud$rho23 * (LV23[t] + z23[t] + b23[t] - country_yld2[t, 9 ]) +
            lfp20_24 * cud$rho24 * (LV24[t] + z24[t] + b24[t] - country_yld2[t, 10]) +
            lfp25_29 * cud$rho25 * (LV25[t] + z25[t] + b25[t] - country_yld2[t, 11]) +
            lfp25_29 * cud$rho26 * (LV26[t] + z26[t] + b26[t] - country_yld2[t, 12]) +
            lfp25_29 * cud$rho27 * (LV27[t] + z27[t] + b27[t] - country_yld2[t, 13]) +
            lfp25_29 * cud$rho28 * (LV28[t] + z28[t] + b28[t] - country_yld2[t, 14]) +
            lfp25_29 * cud$rho29 * (LV29[t] + z29[t] + b29[t] - country_yld2[t, 15]) +
            lfp30_34 * cud$rho30 * (LV30[t] + z30[t] + b30[t] - country_yld2[t, 16]) +
            lfp30_34 * cud$rho31 * (LV31[t] + z31[t] + b31[t] - country_yld2[t, 17]) +
            lfp30_34 * cud$rho32 * (LV32[t] + z32[t] + b32[t] - country_yld2[t, 18]) +
            lfp30_34 * cud$rho33 * (LV33[t] + z33[t] + b33[t] - country_yld2[t, 19]) +
            lfp30_34 * cud$rho34 * (LV34[t] + z34[t] + b34[t] - country_yld2[t, 20]) +
            lfp35_39 * cud$rho35 * (LV35[t] + z35[t] + b35[t] - country_yld2[t, 21]) +
            lfp35_39 * cud$rho36 * (LV36[t] + z36[t] + b36[t] - country_yld2[t, 22]) +
            lfp35_39 * cud$rho37 * (LV37[t] + z37[t] + b37[t] - country_yld2[t, 23]) +
            lfp35_39 * cud$rho38 * (LV38[t] + z38[t] + b38[t] - country_yld2[t, 24]) +
            lfp35_39 * cud$rho39 * (LV39[t] + z39[t] + b39[t] - country_yld2[t, 25]) +
            lfp40_44 * cud$rho40 * (LV40[t] + z40[t] + b40[t] - country_yld2[t, 26]) +
            lfp40_44 * cud$rho41 * (LV41[t] + z41[t] + b41[t] - country_yld2[t, 27]) +
            lfp40_44 * cud$rho42 * (LV42[t] + z42[t] + b42[t] - country_yld2[t, 28]) +
            lfp40_44 * cud$rho43 * (LV43[t] + z43[t] + b43[t] - country_yld2[t, 29]) +
            lfp40_44 * cud$rho44 * (LV44[t] + z44[t] + b44[t] - country_yld2[t, 30]) +
            lfp45_49 * cud$rho45 * (LV45[t] + z45[t] + b45[t] - country_yld2[t, 31]) +
            lfp45_49 * cud$rho46 * (LV46[t] + z46[t] + b46[t] - country_yld2[t, 32]) +
            lfp45_49 * cud$rho47 * (LV47[t] + z47[t] + b47[t] - country_yld2[t, 33]) +
            lfp45_49 * cud$rho48 * (LV48[t] + z48[t] + b48[t] - country_yld2[t, 34]) +
            lfp45_49 * cud$rho49 * (LV49[t] + z49[t] + b49[t] - country_yld2[t, 35]) +
            lfp50_54 * cud$rho50 * (LV50[t] + z50[t] + b50[t] - country_yld2[t, 36]) +
            lfp50_54 * cud$rho51 * (LV51[t] + z51[t] + b51[t] - country_yld2[t, 37]) +
            lfp50_54 * cud$rho52 * (LV52[t] + z52[t] + b52[t] - country_yld2[t, 38]) +
            lfp50_54 * cud$rho53 * (LV53[t] + z53[t] + b53[t] - country_yld2[t, 39]) +
            lfp50_54 * cud$rho54 * (LV54[t] + z54[t] + b54[t] - country_yld2[t, 40]) +
            lfp55_59 * cud$rho55 * (LV55[t] + z55[t] + b55[t] - country_yld2[t, 41]) +
            lfp55_59 * cud$rho56 * (LV56[t] + z56[t] + b56[t] - country_yld2[t, 42]) +
            lfp55_59 * cud$rho57 * (LV57[t] + z57[t] + b57[t] - country_yld2[t, 43]) +
            lfp55_59 * cud$rho58 * (LV58[t] + z58[t] + b58[t] - country_yld2[t, 44]) +
            lfp55_59 * cud$rho59 * (LV59[t] + z59[t] + b59[t] - country_yld2[t, 45]) +
            lfp60_64 * cud$rho60 * (LV60[t] + z60[t] + b60[t] - country_yld2[t, 46]) +
            lfp60_64 * cud$rho61 * (LV61[t] + z61[t] + b61[t] - country_yld2[t, 47]) +
            lfp60_64 * cud$rho62 * (LV62[t] + z62[t] + b62[t] - country_yld2[t, 48]) +
            lfp60_64 * cud$rho63 * (LV63[t] + z63[t] + b63[t] - country_yld2[t, 49]) +
            lfp60_64 * cud$rho64 * (LV64[t] + z64[t] + b64[t] - country_yld2[t, 50]) +
            lfpGE65  * cud$rho65 * (LV65[t] + z65[t] + b65[t] - country_yld2[t, 51]) +
            lfpGE65  * cud$rho66 * (LV66[t] + z66[t] + b66[t] - country_yld2[t, 52]) +
            lfpGE65  * cud$rho67 * (LV67[t] + z67[t] + b67[t] - country_yld2[t, 53]) +
            lfpGE65  * cud$rho68 * (LV68[t] + z68[t] + b68[t] - country_yld2[t, 54]) +
            lfpGE65  * cud$rho69 * (LV69[t] + z69[t] + b69[t] - country_yld2[t, 55]) +
            0 * (LV70[t] + z70[t] + b70[t]) +
            0 * (LV71[t] + z71[t] + b71[t]) +
            0 * (LV72[t] + z72[t] + b72[t]) +
            0 * (LV73[t] + z73[t] + b73[t]) +
            0 * (LV74[t] + z74[t] + b74[t]) +
            0 * (LV75[t] + z75[t] + b75[t]) +
            0 * (LV76[t] + z76[t] + b76[t]) +
            0 * (LV77[t] + z77[t] + b77[t]) +
            0 * (LV78[t] + z78[t] + b78[t]) +
            0 * (LV79[t] + z79[t] + b79[t]) +
            0 * (LV80[t] + z80[t] + b80[t]) +
            0 * (LV90[t] + z90[t] + b90[t]) +
            cud$rho15 * (ifelse(t > 13, 1, 0) * z0[max(1, t - 13)] +
                           ifelse(t > 5, 1, 0) * z5[max(1, t - 5)]) +
            cud$rho30 * (q1 * (b0[t] + b1[t] + b2[t] + b3[t] + b4[t])
                         + q2 * (b5[t] + b6[t] + b7[t] + b8[t] + b9[t]
                                 + b10[t] + b11[t] + b12[t] + b13[t] + b14[t]))

          ## ADDITIONAL OUTCOME = WORKING AGE POP # edited ACP 18-Aug
          WP[t] <-
            (LV15[t] + z15[t] + b15[t]) +
            (LV16[t] + z16[t] + b16[t]) +
            (LV17[t] + z17[t] + b17[t]) +
            (LV18[t] + z18[t] + b18[t]) +
            (LV19[t] + z19[t] + b19[t]) +
            (LV20[t] + z20[t] + b20[t]) +
            (LV21[t] + z21[t] + b21[t]) +
            (LV22[t] + z22[t] + b22[t]) +
            (LV23[t] + z23[t] + b23[t]) +
            (LV24[t] + z24[t] + b24[t]) +
            (LV25[t] + z25[t] + b25[t]) +
            (LV26[t] + z26[t] + b26[t]) +
            (LV27[t] + z27[t] + b27[t]) +
            (LV28[t] + z28[t] + b28[t]) +
            (LV29[t] + z29[t] + b29[t]) +
            (LV30[t] + z30[t] + b30[t]) +
            (LV31[t] + z31[t] + b31[t]) +
            (LV32[t] + z32[t] + b32[t]) +
            (LV33[t] + z33[t] + b33[t]) +
            (LV34[t] + z34[t] + b34[t]) +
            (LV35[t] + z35[t] + b35[t]) +
            (LV36[t] + z36[t] + b36[t]) +
            (LV37[t] + z37[t] + b37[t]) +
            (LV38[t] + z38[t] + b38[t]) +
            (LV39[t] + z39[t] + b39[t]) +
            (LV40[t] + z40[t] + b40[t]) +
            (LV41[t] + z41[t] + b41[t]) +
            (LV42[t] + z42[t] + b42[t]) +
            (LV43[t] + z43[t] + b43[t]) +
            (LV44[t] + z44[t] + b44[t]) +
            (LV45[t] + z45[t] + b45[t]) +
            (LV46[t] + z46[t] + b46[t]) +
            (LV47[t] + z47[t] + b47[t]) +
            (LV48[t] + z48[t] + b48[t]) +
            (LV49[t] + z49[t] + b49[t]) +
            (LV50[t] + z50[t] + b50[t]) +
            (LV51[t] + z51[t] + b51[t]) +
            (LV52[t] + z52[t] + b52[t]) +
            (LV53[t] + z53[t] + b53[t]) +
            (LV54[t] + z54[t] + b54[t]) +
            (LV55[t] + z55[t] + b55[t]) +
            (LV56[t] + z56[t] + b56[t]) +
            (LV57[t] + z57[t] + b57[t]) +
            (LV58[t] + z58[t] + b58[t]) +
            (LV59[t] + z59[t] + b59[t]) +
            (LV60[t] + z60[t] + b60[t]) +
            (LV61[t] + z61[t] + b61[t]) +
            (LV62[t] + z62[t] + b62[t]) +
            (LV63[t] + z63[t] + b63[t]) +
            (LV64[t] + z64[t] + b64[t]) +
            (LV65[t] + z65[t] + b65[t]) +
            (LV66[t] + z66[t] + b66[t]) +
            (LV67[t] + z67[t] + b67[t]) +
            (LV68[t] + z68[t] + b68[t]) +
            (LV69[t] + z69[t] + b69[t])

          ## ADDITIONAL OUTCOME = TOTAL POP # edited ACP 23-Aug
          TotPop[t] <-
            (LV0[t] + z0[t] + b0[t]) +
            (LV1[t] + z1[t] + b1[t]) +
            (LV2[t] + z2[t] + b2[t]) +
            (LV3[t] + z3[t] + b3[t]) +
            (LV4[t] + z4[t] + b4[t]) +
            (LV5[t] + z5[t] + b5[t]) +
            (LV6[t] + z6[t] + b6[t]) +
            (LV7[t] + z7[t] + b7[t]) +
            (LV8[t] + z8[t] + b8[t]) +
            (LV9[t] + z9[t] + b9[t]) +
            (LV10[t] + z10[t] + b10[t]) +
            (LV11[t] + z11[t] + b11[t]) +
            (LV12[t] + z12[t] + b12[t]) +
            (LV13[t] + z13[t] + b13[t]) +
            (LV14[t] + z14[t] + b14[t]) +
            (LV15[t] + z15[t] + b15[t]) +
            (LV16[t] + z16[t] + b16[t]) +
            (LV17[t] + z17[t] + b17[t]) +
            (LV18[t] + z18[t] + b18[t]) +
            (LV19[t] + z19[t] + b19[t]) +
            (LV20[t] + z20[t] + b20[t]) +
            (LV21[t] + z21[t] + b21[t]) +
            (LV22[t] + z22[t] + b22[t]) +
            (LV23[t] + z23[t] + b23[t]) +
            (LV24[t] + z24[t] + b24[t]) +
            (LV25[t] + z25[t] + b25[t]) +
            (LV26[t] + z26[t] + b26[t]) +
            (LV27[t] + z27[t] + b27[t]) +
            (LV28[t] + z28[t] + b28[t]) +
            (LV29[t] + z29[t] + b29[t]) +
            (LV30[t] + z30[t] + b30[t]) +
            (LV31[t] + z31[t] + b31[t]) +
            (LV32[t] + z32[t] + b32[t]) +
            (LV33[t] + z33[t] + b33[t]) +
            (LV34[t] + z34[t] + b34[t]) +
            (LV35[t] + z35[t] + b35[t]) +
            (LV36[t] + z36[t] + b36[t]) +
            (LV37[t] + z37[t] + b37[t]) +
            (LV38[t] + z38[t] + b38[t]) +
            (LV39[t] + z39[t] + b39[t]) +
            (LV40[t] + z40[t] + b40[t]) +
            (LV41[t] + z41[t] + b41[t]) +
            (LV42[t] + z42[t] + b42[t]) +
            (LV43[t] + z43[t] + b43[t]) +
            (LV44[t] + z44[t] + b44[t]) +
            (LV45[t] + z45[t] + b45[t]) +
            (LV46[t] + z46[t] + b46[t]) +
            (LV47[t] + z47[t] + b47[t]) +
            (LV48[t] + z48[t] + b48[t]) +
            (LV49[t] + z49[t] + b49[t]) +
            (LV50[t] + z50[t] + b50[t]) +
            (LV51[t] + z51[t] + b51[t]) +
            (LV52[t] + z52[t] + b52[t]) +
            (LV53[t] + z53[t] + b53[t]) +
            (LV54[t] + z54[t] + b54[t]) +
            (LV55[t] + z55[t] + b55[t]) +
            (LV56[t] + z56[t] + b56[t]) +
            (LV57[t] + z57[t] + b57[t]) +
            (LV58[t] + z58[t] + b58[t]) +
            (LV59[t] + z59[t] + b59[t]) +
            (LV60[t] + z60[t] + b60[t]) +
            (LV61[t] + z61[t] + b61[t]) +
            (LV62[t] + z62[t] + b62[t]) +
            (LV63[t] + z63[t] + b63[t]) +
            (LV64[t] + z64[t] + b64[t]) +
            (LV65[t] + z65[t] + b65[t]) +
            (LV66[t] + z66[t] + b66[t]) +
            (LV67[t] + z67[t] + b67[t]) +
            (LV68[t] + z68[t] + b68[t]) +
            (LV69[t] + z69[t] + b69[t]) +
            (LV70[t] + z70[t] + b70[t]) +
            (LV71[t] + z71[t] + b71[t]) +
            (LV72[t] + z72[t] + b72[t]) +
            (LV73[t] + z73[t] + b73[t]) +
            (LV74[t] + z74[t] + b74[t]) +
            (LV75[t] + z75[t] + b75[t]) +
            (LV76[t] + z76[t] + b76[t]) +
            (LV77[t] + z77[t] + b77[t]) +
            (LV78[t] + z78[t] + b78[t]) +
            (LV79[t] + z79[t] + b79[t]) +
            (LV80[t] + z80[t] + b80[t]) +
            (LV90[t] + z90[t] + b90[t])

          A[t]  <- A[1] * exp(sum(g_t[1:t]))  # edited NAM Aug-11
          H[t]  <- H[1] * exp(sum(h_t[1:t]))  # edited NAM Aug-11
          K[t]  <- s_t[t] * Y[t - 1] - r * C[t - 1] + (1 - delta_t[t]) * K[t - 1]  # edited NAM Aug-11
          Y0[t] <- A[t] * (K[t]^alpha_t[t]) * ((H[t] * L[t])^(1 - alpha_t[t]))  # edited NAM Aug-11
          NP    <- gdp2020 / (Y0[2] * 1000000)
          Y[t]  <- NP * A[t] * (K[t]^alpha_t[t]) * ((H[t] * L[t])^(1 - alpha_t[t]))  # edited NAM Aug-11
          y[t]  <- Y[t] / L[t]
          year[t] <- 2018 + t
          # Always check that the dynamic component is working
          test[t] <-
            ifelse(t > 13, 1, 0) * z0[max(1, t - 13)] + ifelse(t > 5, 1, 0) * z5[max(1, t - 5)]
          gain[t] <- Y[t] - Y_B[t]
          percentagegain[t] <- 100 * (Y[t] - Y_B[t]) / Y_B[t]
        }

        cbind(year, gain, percentagegain, Y_B, WP, WP_B, TotPop, TotPop_B)[2:62, ] # edited NAM Aug-17 # edited ACP 23-Aug
      }

      # Now replicate the function as many times as wanted

      # set.seed(123)
      # Input derived values from above as arguments of cipe function
      # edited NAM Aug-17
      res_na <- TRUE
      add_seed <- 0
      while( res_na & add_seed<10 ){
        set.seed(j0+add_seed*1000)
        simul <- replicate(1, cipe(
          alpha_vals = inverse_logit(macro_par_means$alpha[i]+macro_par_resid$alpha), # edited NAM Aug-11, then Aug-13, then Aug-25
          g_vals     = macro_par_means$g[i]+macro_par_resid$g,           # edited NAM Aug-11, then Aug-25
          s_vals     = inverse_logit(macro_par_means$s[i]+macro_par_resid$s),     # edited NAM Aug-11, then Aug-25
          delta_vals = inverse_logit(macro_par_means$delta[i]+macro_par_resid$delta),       # edited NAM Aug-11, then Aug-25
          h_vals     = macro_par_means$h[i]+macro_par_resid$h            # edited NAM Aug-11, then Aug-25
        ), simplify = FALSE )

        if(sum(is.na(simul)==0)){ res_na <- FALSE  }
        if(res_na){ add_seed <- add_seed + 1  }
      }

      simul_long <- do.call(rbind.data.frame, simul) ## Save as long-form dataframe
      simul_all <- cbind(cty$country,i,cty$region,cty$income,j,Runtype_scenario,simul_long) ## Combine with country, iso3c, UID, and delivery scenario
      ## Re-name data frame # edited ACP 25-Aug
      colnames(simul_all) <- c("country","code","region","income","UID","scenario","year","gain","percentagegain","base_GDP","workingagepop","base_workingagepop","totpop","base_totpop")

      output_all <- rbind(output_all,simul_all) ## Append to overall output dataframe

    }

  }

  ## Save output for each country # edited NAM Aug-17
  write.csv(output_all,paste0("/n/holyscratch01/menzies_lab/Lab/AllisonEconOutput/macro/",i,"_macro_s1.csv"),row.names=FALSE)

}

tbmacro(index)

