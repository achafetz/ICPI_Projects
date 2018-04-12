##   Project:  Malawi Partner Map
##   Autors:   A.Chafetz
##   Purpose:  create underlying map data for COP showing EQUIP/Baylor working districts
##   Date:     2018-04-12
##   Updated:  


# Dependencies ------------------------------------------------------------

#library(ICPIutilities)
library(tidyverse)


# Import ------------------------------------------------------------------

  #import psnu
    df_mwi <- read_rds(Sys.glob(paste0("~/ICPI/Data/ICPI_MER_Structured_Dataset_PSNU_2*.Rds"))) %>% 
      filter(operatingunit == "Malawi")


# Assign Districts to lists for color/shading -----------------------------
  
  #MAP 1
    p_sat <- c("Mangochi District", "Machinga District", "Zomba District", "Chikwawa District", 
               "Lilongwe District", "Phalombe District", "Mulanje District")
    
    p_sus <- c("Salima District", "Balaka District", "Chitipa District", "Karonga District", 
               "Kasungu District", "Nsanje District", "Dowa District", "Nkhotakota District")
    
  #MAP 2
    baylor_sat <- c("Mangochi District", "Machinga District", "Phalombe District", "Mulanje District")
    baylor_sus <- c("Salima District", "Balaka District")
    
    equip_sat <- c("Lilongwe District", "Chikwawa District")
    equip_sus <- c("Chitipa District", "Karonga District", "Kasungu District", "Nsanje District", 
                   "Dowa District", "Nkhotakota District")

# Filter and Mutate -------------------------------------------------------

    
  df_export <- df_mwi %>% 
    filter(!is.na(fy2018q1), fy2018q1 != 0,
           !is.na(psnu), psnu != "_Military Malawi") %>% 
    distinct(psnu, psnuuid, currentsnuprioritization)%>% 
    mutate(shade_map1 = case_when(psnu %in% p_sat      ~ 1L,
                                  psnu %in% p_sus      ~ 2L,
                                  TRUE                 ~ 0L),
           shade_map2 = case_when(psnu %in% baylor_sat ~ 1L,
                                  psnu %in% baylor_sus ~ 2L,
                                  psnu %in% equip_sat  ~ 3L,
                                  psnu %in% equip_sus  ~ 4L,
                                  TRUE                 ~ 0L)
           ) %>% 
    arrange(psnu) 
    

# Export ------------------------------------------------------------------

    
  write_csv(df_export, "~/tmp/MWI/Malawi_map_request_data.csv")


