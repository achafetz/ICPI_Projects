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
    
    df_mer[is.na(df_mer)] <- 0
    
#identify all results data element uids and category option combos possible
  mcad <- df_mer %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR")) %>%
    mutate(fy2017cum = ifelse(indicator=="TX_CURR", fy2017q3, fy2017q1 + fy2017q2 + fy2017q3)) %>% 
    group_by(indicator, indicatortype, standardizeddisaggregate, dataelementuid, ismcad, categoryoptioncomboname, categoryoptioncombouid, age, sex, resultstatus, modality, otherdisaggregate) %>%
    summarize_at(vars(fy2017cum), funs(sum(., na.rm=TRUE))) %>%
    select(indicator, indicatortype, standardizeddisaggregate, dataelementuid,ismcad, categoryoptioncomboname, categoryoptioncombouid, age, sex, resultstatus, modality, otherdisaggregate, fy2017cum)%>%
    filter(fy2017cum != 0) %>%
#export file  
    write_tsv("Downloads/hts_tx_uids_catoptcombo.txt")

