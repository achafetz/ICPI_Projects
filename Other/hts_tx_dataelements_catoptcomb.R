##  HTS_TST, HTS_TST_POS & TX CURR Data Element UIDs and Category Option Combos
##  A.Chafetz, USAID
##  Purpose: pull all relevant info for HTS and TX for EA mapping
##  Date: 10/12/17 

#initialize
  library("tidyverse")
  setwd("C:/Users/achafetz/")

#import Fact View Q3v2.1 dataset
  df_mer  <- read_tsv("Documents/ICPI/Data/ICPI_FactView_OU_IM_20170922_v2_1.txt")
    names(df_mer) <- tolower(names(df_mer)) #convert names to lower case

#identify all data element uids and category option combos possible
  mcad <- df_mer %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR")) %>%
    distinct(indicator, indicatortype, standardizeddisaggregate, dataelementuid, ismcad, categoryoptioncomboname, categoryoptioncombouid, age, sex, resultstatus, modality, otherdisaggregate) %>% 
    select(indicator, indicatortype, standardizeddisaggregate, dataelementuid,ismcad, categoryoptioncomboname, categoryoptioncombouid, age, sex, resultstatus, modality, otherdisaggregate) %>%
#export file  
    write_tsv("Downloads/hts_tx_uids_catoptcombo.txt")

