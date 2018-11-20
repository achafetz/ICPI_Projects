library(tidyverse)
library(readxl)

df_mwi <- read_rds(Sys.glob("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17*.Rds"))  %>% 
  filter(operatingunit == "Malawi")

mech_info <- df_mwi %>% 
  filter((mechanismid %in% c("18234", "18544","17585", "18142","18045", "16704", "18654", "18479")) |
         (mechanismid == "14441" & psnu == "Lilongwe District") |
         (mechanismid == "18025" & psnu != "Lilongwe District") 
          ) %>% 
  select(region:implementingmechanismname) %>% 
  distinct()

ind_info <- df_mwi %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         indicatortype == "DSD") %>% 
  select(indicator:ismcad) %>% 
  distinct()

df_convert <- read_excel("C:/Users/achafetz/Downloads/MwCC-FY18q3 to SGAC.xlsx", sheet = "MwCC")

df_convert <- df_convert %>%
  rename_all(~ str_remove(., "Sum.of.")) %>%
  rename_all(~ str_replace(., "-Targets", "_Targets")) %>% 
  rename_all(~str_replace(., "FY18", "FY2018")) %>% 
  select(-`IP Type`, -contains("Achievement"), -contains("Yield"), -contains("Proxy"), -contains("TX_CURR")) %>% 
  gather(var, val, -PSNU, -Implementing.partner, na.rm = TRUE) %>% 
  separate(var, into = c("indicator", "pd"), sep = "-", remove = TRUE) %>% 
  spread(pd, val) %>% 
  rename_all(~tolower(.))


im_map <- tibble::tribble(
  ~mechanismid,        ~implementing.partner,
        "18234",                      "EQUIP",
        "18544",                      "EGPAF",
        "17585",                     "FHI360",
        "18142",                        "PSI",
        "18045",                    "JHPIEGO",
        "18025",                 "Lighthouse",
        "14441",                 "Lighthouse",
        "16704",              "Johns Hopkins",
        "18654",                        "JSI",
        "18479", "Marie Stopes International")
  



df_convert <- left_join(df_convert, im_map)

df_convert <- df_convert %>% 
  select(-implementing.partner)
df_convert <- right_join(ind_info, df_convert)
df_convert <- right_join(mech_info, df_convert)

df_convert <- df_convert %>% 
  mutate(fy2017_targets = as.double(NA), 
         fy2017q1 = as.double(NA), 
         fy2017q2 = as.double(NA), 
         fy2017q3 = as.double(NA), 
         fy2017q4 = as.double(NA),
         fy2017apr = as.double(NA),
         fy2019_targets = as.double(NA))

headers <- names(df_mer)

df_convert <- df_convert %>% 
  select(headers)
