## Project: TZA VMMC Pull
## Author:  A.Chafetz | USAID
## Purpose: Age results and target used for projecting
## Date:    2019-01-23

library(tidyverse)

df_tza_vmmc <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.rds") %>% 
  filter(operatingunit == "Tanzania",
         indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Age/Sex")

df_psnu <- df_tza_vmmc %>% 
  group_by(psnu, ageasentered) %>% 
  summarize_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(type, val, -psnu, -ageasentered) %>%
  mutate(type = str_to_upper(type)) %>% 
  spread(ageasentered, val, fill = 0) %>% 
  arrange(type, psnu) 

walk(.x = unique(df_psnu$type),
     .f = ~ filter(df_psnu, type == .x) %>% 
            write_csv(paste0("../Downloads/TZA_VMMC_PSNU_", .x, ".csv", na = "")))
  