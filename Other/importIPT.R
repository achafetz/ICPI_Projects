##  Project:  Combine Facility IPT Data
##  Author:   A.Chafetz | USAID
##  Purpose:  Read in IPT data from 60+ sheets and combine into 1 dataframe
##  Date:     2018-12-13



# SETUP -------------------------------------------------------------------

  #dependences
    library(tidyverse)
    library(readxl)
    library(lubridate)

  #identify filepath for IPT file
    filepath <- "../Downloads/Patient_level_IPT.xlsx"
    outputpath <- "../Downloads/"


    

# FUNCTION ----------------------------------------------------------------

readipt <- function(path, sht){
  
  num.columns <- read_xlsx(path, sht, na = "NULL") %>% 
    length()
  
  read_xlsx(path, sht, na = "NULL", col_types = rep("text", num.columns))
  
}
        
# IMPORT ------------------------------------------------------------------

    df_ipt_full <- map_dfr(.x = excel_sheets(filepath),
                           .f = ~ readipt(filepath, .x))
    
    sitemapping <- read_csv("../Downloads/BHT_Sites_Districts.csv") %>% 
      rename(facility_name = FacilityName, district = District)
    
# MUNGE -------------------------------------------------------------------

  
    df_ipt_full <- df_ipt_full %>% 
      mutate_at(vars(patient_id, birth_year, birth_month, birthdate, birthdate_estimated,
                     visit_date, IPT_prescription, IPT_dosage, IPT_dispensed_quantity,
                     year_ART_started, month_ART_started, date_ART_started, next_visit_date,
                     date_enrolled, outcome_date), ~ as.numeric(.))
    
    df_ipt_full <- df_ipt_full %>% 
      mutate_at(vars(birthdate, visit_date, date_ART_started, next_visit_date,
                     date_enrolled, outcome_date), 
                ~ as.Date(., origin = "1899-12-30"))
    
    
    
# MER CALCULATION ---------------------------------------------------------


    df_ipt_full <- df_ipt_full %>% 
      group_by(patient_id) %>% 
      mutate(startdate = min(visit_date, na.rm = TRUE),
             art_status = ifelse(startdate < (date_ART_started + days(90)), "New", "Previous")) %>% 
      ungroup() %>% 
      group_by(facility_name, current_district, startdate, patient_id, gender, birthdate, art_status) %>% 
      summarise_at(vars(IPT_dispensed_quantity), sum, na.rm = TRUE) %>% 
      ungroup %>% 
      mutate(finishe6mo = ifelse(IPT_dispensed_quantity >= 180,1,0), 
             age = as.period(interval(birthdate, startdate), unit = "year") %>% year(.))
    
    
    df_ipt_full <- df_ipt_full %>%   
      mutate(agegroup = case_when(age <  1 ~ "<1",
                                  age <  9 ~ "1-9",
                                  age < 15 ~ "10-14",
                                  age < 20 ~ "15-19",
                                  age < 25 ~ "20-24",
                                  age < 30 ~ "25-29",
                                  age < 35 ~ "30-34",
                                  age < 40 ~ "35-39",
                                  age < 50 ~ "40-49",
                                  age > 49 ~ "50+"))
    
    df_ipt_full <- df_ipt_full %>% 
      mutate(denom = ifelse((startdate >= ymd("2017-04-01") & startdate < ymd("2018-04-01")), 1, 0)) %>% 
      filter(denom == 1) %>% 
      group_by(facility_name, art_status, agegroup, gender) %>% 
      summarise_at(vars(finishe6mo, denom), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      arrange(facility_name, art_status, gender,  agegroup) %>% 
      rename(num = finishe6mo)
    
  
    df_ipt_full <- df_ipt_full %>% 
      mutate(facility_name = str_replace_all(facility_name, " ", "_")) %>% 
      left_join(sitemapping, by = "facility_name") %>% 
      arrange(district, facility_name, art_status, gender,  agegroup)

    df_ipt_full %>% 
      count(art_status, wt = num) %>%
      filter(n != 0)
      print(n = Inf)
    
  
  #export
    write_csv(df_ipt_full, file.path(outputpath, "FY18_TB_IPT_age_sex_status.csv"), na = "")

    