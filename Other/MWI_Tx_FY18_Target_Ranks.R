##   Malawi Site Prioritization
##   A.Chafetz, USAID
##   Purpose: rank Malawi's sites by target volume in the acceleration districts
##   Date: Feb 6, 2018

# L.Brennan - For each district I want to rank sites by TX new and TX Curr to 
#    know which sites are the highest priority for our community programs. We 
#    did this in COP 16 but I want to update based on COP 17 targets to see if 
#    we have any new high burden catchment areas to cover with community programs.

# Variables - TX_CURR, TX_NEW
# Districts - Machinga, Zomba, Blantyre, Mangochi and Chikwawa
# Ask - rank sites by TX_NEW & TX_CURR FY18 targets



library(tidyverse)

#import
df_mwi <- read_tsv("~/ICPI/Data/ICPI_FactView_Site_IM_Malawi_20171222_v2_2.txt") %>% 
  rename_all(tolower)

#munge and create ranking
df_rank <- df_mwi %>% 
   filter((indicator %in% c("TX_CURR", "TX_NEW")), 
           standardizeddisaggregate == "Total Numerator",
           psnu %in% c("Machinga District", "Zomba District", "Blantyre District", "Mangochi District", "Chikwawa District"),
           fy2018_targets != 0, !is.na(fy2018_targets)) %>% 
  select(psnu, orgunituid, facility, currentfacilityprioritization,fundingagency, mechanismid, primepartner, 
         implementingmechanismname, indicator, fy2018_targets) %>% 
  spread(indicator, fy2018_targets) %>% 
  rename_all(tolower) %>% 
  rename(tx_curr_fy18_targets = tx_curr, 
             tx_new_fy18_targets = tx_new) %>% 
  group_by(psnu) %>% 
  mutate(tx_curr_rank = row_number(desc(tx_curr_fy18_targets)),
         tx_new_rank = row_number(desc(tx_new_fy18_targets))) %>% 
  arrange(psnu, tx_curr_rank) %>% 
  select(psnu, facility, orgunituid, currentfacilityprioritization:tx_curr_fy18_targets, tx_curr_rank, tx_new_fy18_targets, tx_new_rank)

#export
  write_csv(df_rank, "~/tmp/MWI_site_ranks.csv", na = "")
  
 
