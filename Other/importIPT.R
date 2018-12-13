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


# IMPORT ------------------------------------------------------------------

  #import
    df_ipt <- map_dfr(.x = excel_sheets(filepath),                  
                      .f = ~ read_xlsx(filepath, sheet = .x, na = "NULL"))

  #export
    write_csv(df_ipt, file.path(outputpath, "patient_ipt_combo.csv"), na = "")
    openxlsx::write.xlsx(df_ipt, file.path(outputpath, "patient_ipt_combo.xlsx"))

