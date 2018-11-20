##   Malawi Quarterly Site Trends
##   FY18 Q4 POART Prep
##   A.Chafetz, USAID
##   Purpose: provide site trends for key indicators and certain districts
##   Date: 2018-11-20

##   Data Source: DATIM Genie Site x IM (2018-11-20): Malawi, USAID

# REQUEST -----------------------------------------------------------------

# Anteneh is looking for a data pull with the information below by quarter
# Indicators of Interest	    Districts
# HTS_TST (index testing)	    Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe
# TST_POS (index testing)	    Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe
# HTS_TST                     all districts
# HTS_TST_POS	                Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe
# TX_NEW	                    all districts
# TX_CURR	                    all districts
# TB_ART	                    all districts


# SETUP -------------------------------------------------------------------

#dependencies
library(tidyverse)
library(openxlsx)
library(fs)
library(ICPIutilities)

#import data
df_mwi <- match_msd("C:/Users/achafetz/Downloads/PEPFAR-Data-Genie-SiteByIMs-2018-11-20.zip")

#PSNUs of interest for certian indicators
psnus <- c("Blantyre District",
           "Chikwawa District",
           "Zomba District",
           "Mangochi District",
           "Machinga District",
           "Lilongwe District")

# MUNGE - SITE ------------------------------------------------------------

#index testing (select PSNUS)
df_index <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         modality %in% c("Index", "IndexMod"),
         psnu %in% psnus) %>%
  group_by(psnu, sitename, indicator, mechanismid, implementingmechanismname) %>%
  summarise_at(vars(fy2018_targets, contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(indicator = paste0(indicator, "_INDEX")) %>%
  filter_at(vars(fy2018_targets, contains("q")), any_vars(.!=0))

#Pos testing (select PSNUS)
df_hts_pos <- df_mwi %>%
  filter(indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Total Numerator",
         psnu %in% psnus) %>%
  group_by(psnu, sitename, indicator, mechanismid, implementingmechanismname) %>%
  summarise_at(vars(fy2018_targets, contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter_at(vars(fy2018_targets, contains("q")), any_vars(.!=0))

#HTS_TST, TX_NEW, TX_CURR, TB_ART (all districts)
df_other <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "TX_NEW", "TX_CURR", "TB_ART"),
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(psnu, sitename, indicator, mechanismid, implementingmechanismname) %>%
  summarise_at(vars(fy2018_targets, contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter_at(vars(fy2018_targets, contains("q")), any_vars(.!=0))

#combine
df_site <- bind_rows(df_index, df_hts_pos, df_other) %>%
  mutate(indicator = factor(indicator,
                            levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW",
                                       "TX_CURR", "TB_ART", "HTS_TST_INDEX", "HTS_TST_POS_INDEX"))) %>%
  arrange(psnu, sitename, indicator)

df_site <- df_site %>%
  rename(District = psnu, `Facility Name` = sitename,
         `Mechanism ID` = mechanismid, `Mechanism Name` = implementingmechanismname) %>%
  rename_at(vars(starts_with("fy")), ~ toupper(.))

df_site <- df_site %>%
  mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))

# MUNGE - DISTRICT (AGE) --------------------------------------------------

#index testing (select PSNUS)
df_index_d <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         modality %in% c("Index", "IndexMod"),
         psnu %in% psnus) %>%
  mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>%
  filter(agecoarse != "Unknown Age") %>%
  group_by(psnu, indicator, sex, agecoarse) %>%
  summarise_at(vars(starts_with("fy2018q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(indicator = paste0(indicator, "_INDEX")) %>%
  filter_at(vars(starts_with("fy2018q")), any_vars(.!=0))

df_other_d <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TB_ART"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age/Sex/Result"),
         psnu %in% psnus) %>%
  mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>%
  filter(agecoarse != "Unknown Age") %>%
  group_by(psnu, indicator, sex, agecoarse) %>%
  summarise_at(vars(starts_with("fy2018q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter_at(vars(starts_with("fy2018q")), any_vars(.!=0))

df_district <- bind_rows(df_other_d, df_index_d)

df_district <- df_district %>%
  gather(pd, val, starts_with("fy")) %>%
  mutate(pd = str_remove(pd, "fy2018") %>% toupper()) %>%
  unite(disagg, c("pd", "sex", "agecoarse")) %>%
  spread(disagg, val) %>%
  select(District = psnu, indicator, `Q1_Male_15+`, `Q2_Male_15+`, `Q3_Male_15+`, `Q4_Male_15+`,
         `Q1_Female_15+`, `Q2_Female_15+`, `Q3_Female_15+`, `Q4_Female_15+`,
         `Q1_Children_<15`, `Q2_Children_<15`, `Q3_Children_<15`, `Q4_Children_<15`) %>%
  rename_all(~str_replace_all(., "_", " "))

df_district <- df_district %>%
  mutate(indicator = factor(indicator,
                            levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW",
                                       "TX_CURR", "HTS_TST_INDEX", "HTS_TST_POS_INDEX"))) %>%
  arrange(District, indicator)

df_district <- df_district %>%
  mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))


# COMPLETENESS ------------------------------------------------------------

site_completeness <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate %in% c("Total Numerator","Age/Sex/HIVStatus", "Modality/Age/Sex/Result"),
         psnu %in% psnus) %>%
  mutate(standardizeddisaggregate = ifelse(standardizeddisaggregate == "Total Numerator",
                                           "Total Numerator", "Age/Sex")) %>%
  group_by(psnu, sitename, indicator, mechanismid, implementingmechanismname, standardizeddisaggregate) %>%
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  gather(pd, val, contains("q"), na.rm = TRUE) %>%
  spread(standardizeddisaggregate, val) %>%
  filter_at(vars(`Age/Sex`,`Total Numerator`), any_vars(.!=0)) %>%
  mutate(completeness = round(`Age/Sex`/`Total Numerator`, 3)*100)

district_completeness <- df_mwi %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator","Age/Sex/HIVStatus", "Modality/Age/Sex/Result"),
         psnu %in% psnus) %>%
  mutate(standardizeddisaggregate = ifelse(standardizeddisaggregate == "Total Numerator",
                                           "Total Numerator", "Age/Sex")) %>%
  group_by(psnu, indicator, standardizeddisaggregate) %>%
  summarise_at(vars(starts_with("fy2018q")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  gather(pd, val, contains("q"), na.rm = TRUE) %>%
  spread(standardizeddisaggregate, val) %>%
  filter_at(vars(`Age/Sex`,`Total Numerator`), any_vars(.!=0)) %>%
  mutate(completeness = round(`Age/Sex`/`Total Numerator`, 3)*100)

# NOTES -------------------------------------------------------------------

notes_source <- "Data Source: DATIM Genie Site x IM (2018-11-20): Malawi, USAID [DATIM GLOBAL]"
notes_1 <- "Site Trends tab"
notes_psnus <- tibble::tribble(
    ~`Indicators of Interest`,                                              ~Districts,
  "HTS_TST (index testing)", "Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe",
  "TST_POS (index testing)", "Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe",
                  "HTS_TST",                                           "all districts",
              "HTS_TST_POS", "Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe",
                   "TX_NEW",                                           "all districts",
                  "TX_CURR",                                           "all districts",
                   "TB_ART",                                           "all districts"
  )
notes_2 <- "District Age Trends"
notes_3 <- "Only select districts: Blantyre, Chikwawa, Zomba, Mangochi, Machinga, Lilongwe"
notes_4 <- "Unknown Age excluded"
notes_5 <- "* some sites in Lilongwe may not have age/sex disaggregated data."
notes_6 <- "Site & District Disagg Completeness"
notes_7 <- "Completessness indicators the (Modality/)Age/Sex disaggregate's completeness compared to the total numerator. The Site disagg completessness tab is for HTS_TST and HTS_TST_POS index and the District disagg completeness is for age/sex completeness."
notes_8 <- "Code: "

# EXPORT ------------------------------------------------------------------

wb <- createWorkbook()
addWorksheet(wb, "Site Trends")
writeData(wb, "Site Trends", df_site)

addWorksheet(wb, "District Age Trends")
writeData(wb, "District Age Trends", df_district)

addWorksheet(wb, "Site Disagg Completeness")
writeData(wb, "Site Disagg Completeness", site_completeness)

addWorksheet(wb, "District Disagg Completeness")
writeData(wb, "District Disagg Completeness", district_completeness)

addWorksheet(wb, "Notes")
writeData(wb, "Notes", notes_source, startRow = 1)
writeData(wb, "Notes", notes_1, startRow = 3)
writeData(wb, "Notes", notes_psnus, startRow = 4)
writeData(wb, "Notes", notes_2, startRow = 13)
writeData(wb, "Notes", notes_3, startRow = 14)
writeData(wb, "Notes", notes_4, startRow = 15)
writeData(wb, "Notes", notes_5, startRow = 16)
writeData(wb, "Notes", notes_6, startRow = 18)
writeData(wb, "Notes", notes_7, startRow = 19)



(tmp <- dir_create(file_temp()))
saveWorkbook(wb, file = path(tmp, "MWI_FY18Q4_Trends.xlsx"), overwrite = TRUE)



