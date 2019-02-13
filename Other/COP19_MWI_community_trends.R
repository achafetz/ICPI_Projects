## PROJECT:  TREND IN COMMUNITY TESTING
## AUTHOR:   A.Chafetz
## PURPOSE:  To better understand the contributions of community testing to case finding, esp. for men
## DATE:     2019-02-04

library(tidyverse)
library(scales)
library(extrafont)

  #import
  df_mwi <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.rds") %>% 
    filter(operatingunit == "Malawi")
  
  #names to use for the four partners of interest
  names <- tribble(
             ~implementingmechanismname,       ~name,
                "One Community (One C)",     "One C",
                    #       "LINKAGES",  "Linkages",
                   #"AIDS-free Malawi", "AIDS-free",
                              "SIFPO 2",       "PSI")
  
  #pull out mech names for filtering 
  mechs <- pull(names, implementingmechanismname)    
  
  #subset dataset to testing
  df_c_hts <- df_mwi %>% 
    filter(fundingagency == "USAID",
           implementingmechanismname %in% mechs,
           #primepartner != "John Snow Inc (JSI)", #im name also AIDS-free
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    left_join(., names, by = "implementingmechanismname")
  
  
  df_c_hts <- df_c_hts %>% 
    mutate(sex = ifelse(agecoarse == "<15", "Peds", sex)) %>% 
    filter(!agecoarse %in% c("Unknown Age", NA)) %>% 
    group_by(name, indicator, modality, sex, agecoarse) %>% 
    summarise_at(vars(contains("fy2018q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, contains("q")) %>% 
    mutate(val = ifelse(val == 0, NA, val)) %>% 
    spread(indicator, val) %>% 
    mutate(#Positivity = round(HTS_TST_POS/HTS_TST,3),
           type = ifelse(str_detect(modality, "Mod"), "Community", "Facility"),
           pd = str_remove(pd, "fy2018") %>% toupper(.))

  df_c_hts %>% 
    group_by(name, modality) %>% 
    summarise_at(vars(HTS_TST, HTS_TST_POS), sum, na.rm = TRUE) %>%
    filter_at(vars(HTS_TST, HTS_TST_POS), any_vars(. !=0)) %>% 
    ungroup()

# PLOT THEME --------------------------------------------------------------


plot_theme <- function() {
  theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, color = "#595959"),
        panel.grid = element_line(color = "#ebebeb"),
        plot.title = element_text(size = 15, face = "bold", color = "black"),
        plot.caption = element_text(size = 11,  color = "#595959")
        )
}


# VISUALS -----------------------------------------------------------------


#Community v Facility Testing
  
  fac_comm <-  df_c_hts %>% 
    group_by(name, type, pd) %>% 
    summarise_at(vars(HTS_TST, HTS_TST_POS), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>% 
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(grp = paste(name, type, ind),
           lab = case_when(name == "PSI" & ind == "HTS_TST_POS" &
                             pd == "Q4" ~ str_remove(type, "unity"))) 
  
  fac_comm  %>% 
    ggplot(aes(pd, val, group = grp, color = type)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab), 
              vjust = -1, family = "Gill Sans MT",
              na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#7f8c8d")) +
    scale_y_continuous(label = comma) +
    facet_grid(ind ~ name, scales = "free_y") +
    labs(x = "", y = "",
         title = "PSI SAW A MARKED INCREASE IN POSITIVTY AT END OF FY18",
         caption = "Source: FY18Q4c MSD") +
    plot_theme()
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_FvC.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(fac_comm)  
  
#Community Modality
  
  mods <- df_c_hts %>% 
    filter(type == "Community") %>% 
    group_by(name, modality, pd) %>% 
    summarise_at(vars(HTS_TST, HTS_TST_POS), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    filter(!is.na(HTS_TST)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>% 
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(modality = str_remove(modality, "Mod"),
           grp = paste(name, modality, ind),
           lab = case_when(name == "One C" & pd == "Q1" & ind == "HTS_TST_POS" ~ modality)) 
  
  mods %>% 
    ggplot(aes(pd, val, group = grp, color = modality)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab, vjust = ifelse(modality== "Mobile", 1.7, -1)),
              family = "Gill Sans MT", na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#9b59b6", "#1abc9c", "#2ecc71")) +
    scale_y_continuous(label = comma) +
    facet_grid(ind ~ name, scales = "free_y") +
    labs(x = "", y = "",
         title = "PSI SAW EXTREMELY HIGH POSITIVITY FOR BOTH TESTING MODALITIES",
         caption = "Source: FY18Q4c MSD") +
    plot_theme() 
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Modalities.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(mods)
  
#by sex
  sex <- df_c_hts %>% 
    filter(modality == "IndexMod",
           sex != "Peds") %>% 
    group_by(name, modality, sex, pd) %>% 
    summarise_at(vars(HTS_TST, HTS_TST_POS), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>% 
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(grp = paste(name, modality, sex),
           lab = case_when(name == "One C" & pd == "Q1" & ind == "HTS_TST_POS" ~ sex))
  
  sex %>% 
    ggplot(aes(pd, val, group = grp, color = sex)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab, vjust = ifelse(sex != "Peds", 1.7, -1)),
              family = "Gill Sans MT", na.rm = TRUE) +
    scale_color_manual(values = c("#1abc9c", "#9b59b6", "#95a5a6")) +
    scale_y_continuous(label = comma) +
    facet_grid(ind ~ name, scales = "free_y") +
    labs(x = "", y = "",
         title = "SIMILAR COMMUNITY TESTING OF MALES/FEMALES IN LAST TWO QUARTERS",
         caption = "Source: FY18Q4c MSD") +
    plot_theme() 
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Sex.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(sex)
 

# INDEX TESTING TREND -----------------------------------------------------

  
  acceleration <- c("Blantyre District",
                    "Zomba District",
                    "Mangochi District",
                    "Machinga District",
                    "Chikwawa District")
  
  fac_index_5overall <- df_mwi %>% 
    filter(psnu %in% acceleration,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           !str_detect(modality, "Mod|Malnutrition|Ped")) %>% 
    group_by(operatingunit, indicator, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(modality = case_when(modality == "Emergency Ward" ~ "Emerg.",
                                modality == "PMTCT ANC"      ~ "PMTCT",
                                TRUE                         ~ modality),
           index = ifelse(modality == "Index", "yes", "no"),
           grp = modality,
           pd = str_remove(pd, "fy2018") %>% toupper(.))
  
  order <- fac_index_5overall %>% 
    filter(ind == "HTS_TST_POS") %>% 
    group_by(modality) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(val)) %>% 
    pull(modality)
  
  fac_index_5overall <- mutate(fac_index_5overall, modality = factor(modality, order))
  
  fac_index_5overall %>% 
    ggplot(aes(pd, val, group = grp, color = index)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    scale_color_manual(values = c("#95a5a6", "#3498db")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ modality, scales = "free_y") +
    labs(x = "", y = "",
         title = "INDEX TESTING ON THE RISE BUT NOT MASSIVE GAINS",
         caption = "Note: Data only from the 5 Acceleration districts.
         Source: FY18Q4c MSD") +
    plot_theme()
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Fac_Index.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(fac_index_5overall)
  
  
  
  fac_sex <- df_mwi %>% 
    filter(psnu %in% acceleration,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           !str_detect(modality, "Mod|Malnutrition|Ped"),
           agecoarse == "15+") %>% 
    group_by(operatingunit, indicator, sex, agecoarse, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(modality = case_when(modality == "Emergency Ward" ~ "Emerg.",
                                modality == "PMTCT ANC"      ~ "PMTCT",
                                TRUE                         ~ modality),
           index = ifelse(modality == "Index", "yes", "no"),
           grp = case_when(modality == "Index" ~ paste(modality, sex),
                           TRUE                ~ paste("Other", sex)),
           pd = str_remove(pd, "fy2018") %>% toupper(.),
           modality = factor(modality, order),
           lab = case_when(ind == "HTS_TST" & modality == "OtherPITC" & pd == "Q1" ~ sex)) %>% 
    arrange(modality, sex, pd)
    

  fac_sex %>% 
    ggplot(aes(pd, val, group = grp, color = grp)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab), hjust = -.4,
              na.rm = TRUE) +
    scale_color_manual(values = c("#16a085", "#8e44ad", "#1abc9c", "#9b59b6")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ modality, scales = "free_y") +
    labs(x = "", y = "",
         title = "SEEING A SIMILAR TREND IN INDEX TESTING BY SEX",
         caption = "Note: Data only from the 5 Acceleration districts.
         Source: FY18Q4c MSD") +
    plot_theme()
  
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Fac_Index_Sex.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(order, fac_sex)
  
  fac_index_5psnu <- df_mwi %>% 
    filter(psnu %in% acceleration,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    group_by(operatingunit, psnu, indicator, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    group_by(pd, psnu, ind) %>% 
    mutate(share = ifelse(ind != "Positivity (%)", val / sum(val, na.rm = TRUE), NA)) %>% 
    ungroup() %>% 
    filter(modality == "Index") %>% 
    mutate(psnu = str_remove(psnu, " District"),
           grp = paste(psnu),
           pd = str_remove(pd, "fy2018") %>% toupper(.))
  
  order <- fac_index_5psnu %>% 
    filter(ind == "HTS_TST_POS") %>% 
    group_by(psnu) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(val)) %>% 
    pull(psnu)
  
  fac_index_5psnu <- mutate(fac_index_5psnu, psnu = factor(psnu, order))
  
  fac_index_5psnu %>% 
    ggplot(aes(pd, val, group = grp, color = grp)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#9b59b6", "#1abc9c", "#f39c12", "#2ecc71")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ psnu, scales = "free_y", as.table = TRUE) +
    labs(x = "", y = "",
         title = "FACILITY INDEX TESTING ON THE RISE BUT NOT MASSIVE GAINS IN FY18",
         caption = "Note: Data only from the 5 Acceleration districts.
         Source: FY18Q4c MSD") +
    plot_theme()
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Fac_Index_PSNU.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(fac_index_5psnu, order)
  
#Community
  
  comm_index_overall <- df_mwi %>% 
    filter(psnu != "_Military Malawi",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           str_detect(modality, "Mod")) %>% 
    group_by(operatingunit, indicator, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    group_by(pd, ind) %>% 
    mutate(share = ifelse(ind != "Positivity (%)", val / sum(val, na.rm = TRUE), NA),
           modality = str_remove(modality, "Mod")) %>% 
    ungroup() %>% 
    filter(modality != "Home") %>% 
    mutate(grp = paste(modality),
           pd = str_remove(pd, "fy2018") %>% toupper(.))
  
  order <- comm_index_overall %>% 
    filter(ind == "HTS_TST_POS") %>% 
    group_by(modality) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(val)) %>% 
    pull(modality)
  
  comm_index_overall <- mutate(comm_index_overall, modality = factor(modality, order))
  
  comm_index_overall %>% 
    ggplot(aes(pd, val, group = grp, color = grp)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#95a5a6", "#95a5a6", "#95a5a6")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ modality, scales = "free_y") +
    labs(x = "", y = "",
         title = "COMMUNITY TESTING INCREASING POSITIVITY; STILL DOESN'T MATCH MOBILE",
         caption = "Source: FY18Q4c MSD") +
    plot_theme()
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Comm_Index.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(comm_index_overall)
  
  
  
  comm_sex <- df_mwi %>% 
    filter(psnu %in% acceleration,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           str_detect(modality, "Mod"),
           agecoarse == "15+") %>% 
    group_by(operatingunit, indicator, sex, agecoarse, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    mutate(grp = case_when(modality == "IndexMod" ~ paste(modality, sex),
                           TRUE                ~ paste("Other", sex)),
           pd = str_remove(pd, "fy2018") %>% toupper(.),
           modality = str_remove(modality, "Mod"),
           modality = factor(modality, order),
           lab = case_when(ind == "HTS_TST" & modality == "Mobile" & pd == "Q3" ~ sex)) %>% 
    arrange(modality, sex, pd)
  
  
  comm_sex %>% 
    ggplot(aes(pd, val, group = grp, color = grp)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab), vjust = 1.5,
              na.rm = TRUE) +
    scale_color_manual(values = c("#16a085", "#8e44ad", "#1abc9c", "#9b59b6")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ modality, scales = "free_y") +
    labs(x = "", y = "",
         title = "SEEING A SIMILAR TREND IN INDEX TESTING BY SEX",
         caption = "Note: Data only from the 5 Acceleration districts.
         Source: FY18Q4c MSD") +
    plot_theme()
  
  
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Comm_Index_Sex.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  
  
  comm_index_psnu <- df_mwi %>% 
    filter(psnu != "_Military Malawi",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    group_by(operatingunit, psnu, indicator, modality) %>% 
    summarise_at(vars(fy2018q1:fy2018q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(pd, val, starts_with("fy")) %>% 
    spread(indicator, val) %>% 
    mutate_at(vars(HTS_TST, HTS_TST_POS), ~ ifelse(. == 0, NA, .)) %>% 
    mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST*100, 0)) %>%
    gather(ind, val, HTS_TST, HTS_TST_POS, `Positivity (%)`) %>% 
    group_by(pd, psnu, ind) %>% 
    mutate(share = ifelse(ind != "Positivity (%)", val / sum(val, na.rm = TRUE), NA)) %>% 
    ungroup() %>% 
    filter(modality == "IndexMod") %>% 
    mutate(psnu = str_remove(psnu, " District"),
           grp = paste(psnu),
           pd = str_remove(pd, "fy2018") %>% toupper(.))
  
  order <- comm_index_psnu %>% 
    filter(ind == "HTS_TST_POS") %>% 
    group_by(psnu) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(val)) %>% 
    pull(psnu)
  
  comm_index_psnu <- mutate(comm_index_psnu, psnu = factor(psnu, order))
  
  
  comm_index_psnu %>% 
    ggplot(aes(pd, val, group = grp, color = grp)) +
    geom_hline(aes(yintercept = 0), color = "#595959") +
    geom_line(size = 2, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    scale_color_manual(values = c("#3498db", "#9b59b6", "#1abc9c", "#f39c12", "#2ecc71",
                                  "#e74c3c", "#f1c40f", "#2980b9", "#8e44ad", "#d35400",
                                  "#7f8c8d", "#e67e22")) +
    scale_y_continuous(labels = comma) +
    facet_grid(ind ~ psnu, scales = "free_y", as.table = TRUE) +
    labs(x = "", y = "",
         title = "COMMUNITY TESTING TREND",
         caption = "Source: FY18Q4c MSD") +
    plot_theme()
    
    
  ggsave("~/COP19/Malawi/HTS Trends/MWI_TST_Trends_Comm_Index_PSNU.png", units = "in", 
         width = 11, height = 8.5, dpi = 300)
  
  rm(comm_index_psnu, order)
      