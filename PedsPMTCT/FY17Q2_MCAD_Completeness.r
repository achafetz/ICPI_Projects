## Dataset for<15/15+ MCAD including completeness


## DEPENDENT LIBRARIES ##
library("tidyverse")
library("readxl")



setwd("C:/Users/achafetz/Documents/GitHub/PartnerProgress/ExcelOutput")
df_gbl <- read.csv("ICPIFactView_SNUbyIM_6Jun2017_GLOBAL.csv", header = TRUE, sep = ",")

df_gbl_compl <- df_gbl %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW")) %>%
  select(operatingunit, indicator, disagg, fy2015q3, fy2015q4, starts_with("fy2016"), starts_with("fy2017")) %>%
  subset(select= -c(fy2016apr, fy2017q3, fy2017q4, fy2017cum)) %>%
  mutate(disagg_t = ifelse(disagg=="Total", "Total", "MCAD")) %>%
  group_by(operatingunit, indicator, disagg_t) %>%
  summarize_each(funs(sum(., na.rm=TRUE)), starts_with("fy")) %>%
  ungroup()
  
df_gbl_compl <-df_gbl_compl %>% 
  gather(pd, value, starts_with("fy")) %>%
  spread(disagg_t, value) %>%
  mutate(compl = MCAD/Total) %>%
  subset(select = -c(MCAD, Total)) %>%
  spread(pd, compl)

colnames(df_gbl_compl) <-paste(colnames(df_gbl_compl), "compl", sep = "_")
rename()

df_gbl_u15 <- df_gbl %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW"), disagg=="<15/Unknown Sex") %>%
  select(operatingunit, indicator, disagg, fy2015q3, fy2015q4, starts_with("fy2016"), starts_with("fy2017")) %>%
  subset(select= -c(fy2016apr, fy2017q3, fy2017q4, fy2017cum)) %>%
  group_by(operatingunit, indicator) %>%
  summarize_each(funs(sum(., na.rm=TRUE)), starts_with("fy"))%>%
  ungroup()

colnames(df_gbl_u15) <-paste(colnames(df_gbl_u15), "u15", sep = "_")

df_gbl_o15 <- df_gbl %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW"), disagg %in% c("15+/Female", "15+/Male")) %>% 
  select(operatingunit, indicator, disagg, fy2015q3, fy2015q4, starts_with("fy2016"), starts_with("fy2017")) %>%
  subset(select= -c(fy2016apr, fy2017q3, fy2017q4, fy2017cum)) %>%
  group_by(operatingunit, indicator) %>%
  summarize_each(funs(sum(., na.rm=TRUE)), starts_with("fy"))%>%
  ungroup()

colnames(df_gbl_o15) <-paste(colnames(df_gbl_o15), "o15", sep = "_")

df_join <- full_join(df_gbl_u15, df_gbl_o15, by= c("operatingunit_u15"="operatingunit_o15", "indicator_u15"= "indicator_o15"))
df_join <- full_join(df_join, df_gbl_compl, by= c("operatingunit_u15"="operatingunit_compl", "indicator_u15"= "indicator_compl"))

df_join <- rename(df_join, operatingunit = operatingunit_u15)
df_join <- renane(df_join, indicator = indicator_u15)

write.csv(df_join, "C:/Users/achafetz/Downloads/mcadcompl.csv", na="")
