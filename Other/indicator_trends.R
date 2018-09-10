library(tidyverse)
library(lubridate)
library(ICPIutilities)

ind <- c("HTS_TST", "TX_NEW", "PMTCT_EID", "HTS_TST_POS", "PMTCT_STAT", 
         "PMTCT_STAT_POS", "TX_CURR", "PMTCT_ART", "VMMC_CIRC",
         "KP_PREV", "PP_PREV", "OVC_HIVSTAT", "OVC_SERV", "TB_ART", 
         "TB_STAT", "TB_STAT_POS", "TB_ART_D", "TB_STAT_D", "TX_TB")
         #"GEND_GBV", "PMTCT_FO", "TX_RET", "KP_MAT"


df_mer <- read_rds(Sys.glob("~/ICPI/Data/MER_Structured_Dataset_OU_FY17*.Rds"))  

df_trend <- df_mer %>% 
  dplyr::mutate(indicator = ifelse((indicator=="TB_ART" & standardizeddisaggregate=="Total Denominator"),"TB_ART_D",indicator),
                indicator = ifelse((indicator=="TB_STAT" & standardizeddisaggregate=="Total Denominator"),"TB_STAT_D",indicator),
                standardizeddisaggregate = ifelse((indicator %in% c("TB_ART_D", "TB_STAT_D")),"Total Numerator",standardizeddisaggregate)) %>% 
  filter(indicator %in% ind, standardizeddisaggregate == "Total Numerator") %>% 
  rename_official()

df_trend_lng <- df_trend %>% 
  select(-ends_with("apr"), -fy2019_targets) %>% 
  group_by(operatingunit, mechanismid, indicator) %>% 
  summarise_at(vars(starts_with("fy")), ~ sum(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  gather(pd, result, starts_with("fy")) %>% 
  mutate(target = ifelse(str_detect(pd, "targets"), result, NA),
         result = ifelse(str_detect(pd, "targets"), NA, result),
         pd = str_replace(pd, "_targets", "q4")) %>% 
  group_by(operatingunit, indicator, mechanismid, pd) %>% 
  summarise_at(vars(result, target), ~ sum(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate_at(vars(target), ~ifelse(.==0, NA,.)) %>% 
  separate("pd", c("fy", "qtr"), "q") %>% 
  arrange(operatingunit, mechanismid, indicator, fy, qtr)

df_trend_lng <- df_trend_lng %>% 
  fill(target, .direction = "up") %>% 
  group_by(operatingunit, mechanismid, indicator, fy) %>% 
  mutate(result_cum = cumsum(result),
         target_qtrly_lowband = (((as.numeric(qtr)*.25))-.1)*target,
         target_goalband = .2 * target) %>% 
  ungroup() %>% 
  mutate(qtr = paste0("Q",qtr),
         fy = toupper(fy))

semi_ann <- c("KP_PREV", "PP_PREV", "SC_STOCK", "TB_ART", "TB_ART_D", "TB_STAT", "TB_STAT_D")
snapshot <- c("OVC_HIVSTAT", "OVC_SERV", "TB_PREV", "TX_TB")

df_trend_lng <- df_trend_lng %>% 
  mutate(result = ifelse(result == 0, NA, result),
         result_cum = case_when(indicator == "TX_CURR"                               ~ result,
                                indicator %in% c(semi_ann, snapshot) & qtr == "Q3"   ~ as.numeric(NA),
                                indicator %in% snapshot & qtr == "Q4"                ~ result,
                                is.na(result)                                        ~ as.numeric(NA),
                                TRUE                                                 ~ result_cum),
         target_goalband = ifelse(indicator %in% c(semi_ann, snapshot) & qtr %in% c("Q1", "Q3"), NA, target_goalband),
         target = ifelse(qtr == "Q4", target, NA))

mechnames <- df_trend %>% 
  select(mechanismid, primepartner, implementingmechanismname) %>% 
  distinct()

df_output <- left_join(df_trend_lng, mechnames, by = "mechanismid") %>% 
  unite(pd, fy, qtr, sep = "") %>% 
  mutate(date = yq(str_remove(pd, "FY"))  %m-% months(3)) %>% 
  select(operatingunit, mechanismid, primepartner, implementingmechanismname, pd, date, everything())
  

  
  
write_csv(df_output, "C:/Users/achafetz/Downloads/trends.csv", na = "")


