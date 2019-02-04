##  PROJECT: MAP TREATMENT COVERAGE SITES
##  AUTHOR:  A.Chafetz, USAID
##  PURPOSE: Map facility coverage breakdown by agency and partner
##  DATE:    2019-02-04


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(datimvalidation)
library(ICPIutilities)
library(scales)
library(sf)
library(RColorBrewer)
library(extrafont)


# IMPORT COORDINATES ------------------------------------------------------

  #access DATIM
    loadSecrets()

  #url
    baseurl<-"https://datim.org/"
    organisationUnit <- "lZsCb6y0KDX"  #Malawi UID
    url<-paste0(baseurl,"api/organisationUnits?filter=path:like:",organisationUnit, 
                "&fields=id,name,path,coordinates&paging=false")
    
  #API pull from DATIM     
    sites <-URLencode(url) %>%
      httr::GET(httr::timeout(60) #, httr::authenticate(myuser,mypwd())
                ) %>% 
      httr::content("text") %>% 
      jsonlite::fromJSON(flatten=TRUE) %>% 
      purrr::pluck("organisationUnits") %>% 
      tibble::as_tibble()
  

  #separate out lat/long coordinates   
    coords <- sites %>%
      dplyr::select(-path) %>% 
      dplyr::filter(!str_detect(name, "District$|Malawi$"), #remove overall district coordinates files
                    !is.na(coordinates)) %>%
      dplyr::mutate(coordinates = stringr::str_remove_all(coordinates, "\\[|]")) %>% 
      tidyr::separate(coordinates, c("longitude", "latitude"), sep = ",") %>% 
      dplyr::select(facilityuid = id, longitude, latitude) %>% 
      dplyr::mutate_at(vars(longitude, latitude), ~ as.numeric(.))
    
    rm(sites)

# FY19 TX TARGETS ---------------------------------------------------------

  #Import MER data
    df_site <- read_rds("~/ICPI/Data/MER_Structured_Dataset_SITE_IM_FY17-18_20181221_v2_1_Malawi.rds")
  
  #filter to TX and clean up mechanism names
    df_site_tx <- df_site %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator",
             fundingagency != "DOD")
    
    df_site_tx <- df_site_tx %>% 
      group_by(psnu, psnuuid, facilityuid, facility, fundingagency, mechanismid) %>% 
      summarise_at(vars(fy2019_targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(fy2019_targets!=0) %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/")) %>% 
      group_by(facilityuid) %>% 
      mutate(patient_grp = case_when(sum(fy2019_targets) >= 2000 ~ "2,000+",
                                     sum(fy2019_targets) >= 1000 ~ "1,000 - 1,999",
                                     sum(fy2019_targets) >=  500 ~ "500 - 999",
                                     TRUE                        ~ "<500") %>% 
               factor(., c("<500", "500 - 999", "1,000 - 1,999", "2,000+"))) %>% 
      ungroup() %>% 
      filter(!mechanismid %in% c("00000", "00001"))
    
  #clean up partner names
    df_site_tx <- tribble(~mechanismid,                   ~name,
                               "70185",                "Balyor", #Balyor TSP
                               "17341",                   "URC",
                               "18544",                 "EGPAF",
                               "17585",              "Linkages",
                               "18244",               "JHPIEGO",
                               "18025",              "LH 18025",
                               "18234",                 "EQUIP",
                               "70186",                   "PIH",
                               "14441",               "LH 14441") %>% 
      left_join(df_site_tx, ., by = "mechanismid")


# APPLY COORDINATES -------------------------------------------------------

  #add coordinates to sites
    df_site_tx <- df_site_tx %>% 
      left_join(., coords, by = "facilityuid")

    
# CHECK COORDINATE COMPLETENESS -------------------------------------------

  #Overall, how many sites are missing coordinates?
    df_site_tx %>% 
      mutate(has_coord = ifelse(is.na(latitude), "No", "Yes")) %>% 
      distinct(facilityuid, has_coord) %>% 
      count(has_coord) %>% 
      spread(has_coord, n) %>% 
      mutate(missing = percent(No/(No + Yes), accuracy = 1)) %>% 
      mutate_at(vars(Yes, No), ~ comma(.))
    
  #Overall, how many patients are in sites missing coordinates?
    df_site_tx %>% 
      mutate(has_coord = ifelse(is.na(latitude), "No", "Yes")) %>% 
      count(has_coord, wt = fy2019_targets) %>% 
      spread(has_coord, n) %>% 
      mutate(missing = percent(No/(No + Yes), accuracy = 1)) %>% 
      mutate_at(vars(Yes, No), ~ comma(.))
    
  #By patient grouping, how many sites are missing coordinates?
    df_site_tx %>% 
      mutate(has_coord = ifelse(is.na(latitude), "No", "Yes")) %>% 
      distinct(facilityuid, patient_grp, has_coord) %>% 
      count(patient_grp, has_coord) %>% 
      spread(has_coord, n) %>% 
      mutate(missing = percent(No/(No + Yes), accuracy = 1)) %>% 
      mutate_at(vars(Yes, No), ~ comma(.))
  
  #By patient grouping, how many patients are in sites missing coordinates?
    df_site_tx %>% 
      mutate(has_coord = ifelse(is.na(latitude), "No", "Yes")) %>% 
      count(patient_grp, has_coord, wt = fy2019_targets) %>% 
      spread(has_coord, n) %>% 
      mutate(missing = percent(No/(No + Yes), accuracy = 1)) %>% 
      mutate_at(vars(Yes, No), ~ comma(.))
    

# IMPORT SHAPEFILES -------------------------------------------------------

    shp_mwi <- st_read("~/COP19/Malawi/DistrictShapefiles/MalawiDistrictLsib2016July.shp") %>% 
      mutate(psnu = as.character(NAME_1),
             psnuuid = as.character(uid)) %>% 
      select(psnu, psnuuid, geometry)


    
# Create map theme --------------------------------------------------------


  map_theme <- function() {
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour="white"),
          panel.grid.minor = element_line(colour="white"),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 13, face = "bold", color = "#595959"),
          plot.title = element_text(size = 18, face = "bold", color = "#2B8CBE"),
          plot.caption = element_text(size = 10),
          text = element_text(family = "Gill Sans MT",  color = "#595959")
    )
  }
  
  palette <- brewer.pal("GnBu", n = 4)


# MAP ---------------------------------------------------------------------

  #Agency comparison
  ggplot(shp_mwi) +
    geom_sf(fill = "white", color = "gray", na.rm = TRUE) +
    geom_point(data = df_site_tx, aes(longitude, latitude, fill = patient_grp), 
               shape = 21,
               color = "gray",
               alpha = .8,
               size = 2, 
               na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      name = "# on Treatment") +
    labs(title = "Distribution of Sites by Treatment Volume",
         caption = "Source: FY19 Targets from FY18Q4c MSD") +
    facet_wrap(. ~ fundingagency) +
    map_theme()
  
  ggsave("~/COP19/Malawi/MWI_FY19_TX_Agency.png", dpi = 300, units = "in", height = 5, width = 9)
  
  #Partner Comparison
  ggplot(shp_mwi) +
    geom_sf(fill = "white", color = "gray", na.rm = TRUE) +
    geom_point(data = df_site_tx, aes(longitude, latitude, fill = patient_grp), 
               shape = 21,
               color = "gray",
               alpha = .7,
               size = 2, 
               na.rm = TRUE) +
    scale_fill_manual(values = palette, 
                      name = "# on Treatment") +
    labs(title = "Distribution of Sites by Treatment Volume",
         caption = "Source: FY19 Targets from FY18Q4c MSD") +
    facet_wrap(. ~ name, nrow = 2) +
    map_theme()
  
  ggsave("~/COP19/Malawi/MWI_FY19_TX_Partner.png", dpi = 300, units = "in", height = 6, width = 12)
  
        