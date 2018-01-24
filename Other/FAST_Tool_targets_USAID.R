##   FAST TOOL Targets COP FY18
##   A.Chafetz, USAID
##   Purpose: targets to check against FAST Tool
##   Date: Dec 22, 2017
##   Updated: 1/24/18

## DEPENDENCIES
  # ICPI Fact View PSNUxIM w/FY18 Targets
  # COP MATRIX REPORT from FACTSInfo

library(tidyverse)

## IMPORT --------------------------------------------------------------------------------------------------------------------------
  
  df_targets <- read_tsv("~/ICPI/Data/ICPI_FactView_PSNU_IM_20171115_v1_2.txt", 
                     col_types = cols(FY2016_TARGETS = "d",
                                      FY2017_TARGETS = "d",
                                      FY2018_TARGETS = "d")) %>% 
    rename_all(tolower)


## CLEAN/SUBSET DATA ----------------------------------------------------------------------------------------------------------------

#fix issues with different mech & partner names(function at bottom of script)
  df_targets <-cleanup_mechs(df_targets, "~/GitHub/DataPack/RawData/")

#subset
  df_targets <- df_targets %>%
    
    #just indicators of interest (total numerators only) & limit variables
    filter(indicator %in% c("TX_NEW", "TX_CURR", "HTS_TST", "HTS_TST_POS", "PMTCT_STAT", "PMTCT_ART", "OVC_SERV", "VMMC_CIRC", "KP_PREV"),
           disaggregate %in% c("Total Numerator", "KeyPop")) %>% 
    #concatenate KP pop names into indicator name
    mutate(indicator = ifelse((indicator == "KP_PREV" & standardizeddisaggregate == "KeyPop"), paste(indicator, otherdisaggregate, sep = " - "), indicator)) %>%
    #summarize at prioritization/im level
    group_by(operatingunit, primepartner, mechanismid, implementingmechanismname, indicator) %>% 
    summarise_at(vars(fy2016_targets, fy2017_targets, fy2018_targets), funs(sum(., na.rm=TRUE))) %>% 
    ungroup() %>% 
    #change 0s to NA
    mutate_at(vars(fy2016_targets, fy2017_targets, fy2018_targets), funs(ifelse(.==0, NA, .)))

## EXPORT DATA ----------------------------------------------------------------------------------------------------------------
  
  write_csv(df_targets, "~/GitHub/DataPack/TempOutput/PEPFAR_FAST_Targets_2017.12.22.csv", na="")







## CLEANUP MECHANISMS FUNCTION -------------------------------------------------------------------------------------------------------

cleanup_mechs <- function(df_to_clean, report_folder_path, report_start_year = 2014) {
  
  #needed packages
  require(tidyverse, quietly = FALSE)
  require(readxl, quietly = FALSE)
  
  #import official mech and partner names; source: FACTS Info
  df_names <- read_excel(Sys.glob(file.path(report_folder_path,"*Standard COP Matrix Report*.xls")), skip = 1)
  
  #rename variable stubs
  names(df_names) <- gsub("Prime Partner", "primepartner", names(df_names))
  names(df_names) <- gsub("Mechanism Name", "implementingmechanismname", names(df_names))
  
  #figure out latest name for IM and partner (should both be from the same year)
  df_names <- df_names %>%
    
    #rename variables that don't fit pattern
    rename(operatingunit =  `Operating Unit`, mechanismid = `Mechanism Identifier`, 
           primepartner__0 = primepartner, implementingmechanismname__0 = implementingmechanismname) %>% 
    #reshape long
    gather(type, name, -operatingunit, -mechanismid) %>%
    
    #split out type and year (eg type = primeparnter__1 --> type = primepartner,  year = 1)
    separate(type, c("type", "year"), sep="__") %>%
    
    #add year (assumes first year if report is 2014)
    mutate(year = as.numeric(year) + report_start_year) %>%
    
    #drop lines/years with missing names
    filter(!is.na(name)) %>%
    
    #group to figure out latest year with names and keep only latest year's names (one obs per mech)
    group_by(operatingunit, mechanismid, type) %>%
    filter(year==max(year)) %>%
    ungroup() %>%
    
    #reshape wide so primepartner and implementingmechanismname are two seperate columsn to match fact view dataset
    spread(type, name) %>%
    
    #convert mechanism id to string for merging back onto main df
    mutate(mechanismid = as.character(mechanismid)) %>%
    
    #keep only names with mechid and renaming with _F to identify as from FACTS  
    select(mechanismid, implementingmechanismname, primepartner) %>%
    rename(implementingmechanismname_F = implementingmechanismname, primepartner_F = primepartner) 
  
  #match mechanism id type for compatible merge
  df_to_clean <- mutate(df_to_clean, mechanismid = as.character(mechanismid))
  
  #merge in official names
  df_to_clean <- left_join(df_to_clean, df_names, by="mechanismid")
  
  #replace prime partner and mech names with official names
  df_to_clean <- df_to_clean %>%
    mutate(implementingmechanismname = ifelse(is.na(implementingmechanismname_F), implementingmechanismname, implementingmechanismname_F), 
           primepartner = ifelse(is.na(primepartner_F), primepartner, primepartner_F)) %>%
    select(-ends_with("_F"))
}
