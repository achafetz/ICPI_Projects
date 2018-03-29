##   Convert Template
##   A.Chafetz, USAID
##   Purpose: Convert DRC Template into Fact View style
##   Date: 2018.03.29

# Notes
# - template sent by N.Bartlett (https://drive.google.com/file/d/0B0kWHKt_xDVYVURra3ItQmZqaGFfQlFYZ1lFOHF4UUwxZVpZ/view?usp=sharing)
# - this only imports on of tabs; process can be mimics for the others and dfs can be combines
# - useful links
#    - http://r4ds.had.co.nz/strings.html
#    - https://github.com/rstudio/cheatsheets/raw/master/strings.pdf

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)


# Import ------------------------------------------------------------------
  
  #read in template for index testing
    folderpath <- "~/tmp/"
    template_index <- read_excel(file.path(folderpath, "Q2-FY18 Prioritaized Indicators Template.xls"), 
                          sheet = "HTS - Index & IndexMod", skip = 2)

# Reshape -----------------------------------------------------------------

  #reshape long to mimic Fact View & rename org units
    fvstyle <- template_index %>% 
      gather(dataelement_full, fy2018q2, -starts_with("Org")) %>% 
      rename(snu1 = `Org unit level 2`,
             psnu = `Org unit level 3`,
             sitename = `Org unit level 4`)
    
    rm(template_index)

# Data Element Breakout ---------------------------------------------------

  #breakout headers into pieces
    fvstyle <- fvstyle %>% 
      mutate(
        dataelement = str_replace(dataelement_full, "^.*(DSD|TA), ", ""),
        operatingunit = "Democratic Republic of the Congo",
        disaggregate = str_extract(dataelement, "^.*\\)") %>% str_replace("\\)", ""),
        indicator = str_extract(dataelement_full, "^.*\\(") %>% str_replace("\\(", ""),
        numeratordenom = str_extract(dataelement_full, "\\((N|D),") %>% str_replace("\\(", "") %>% str_replace(",", ""),
        indicatortype = str_extract(dataelement_full, "DSD|TA"),
        modality = str_replace(dataelement,"/", "!") %>% str_extract("^.*!") %>%  str_replace("!", ""),
        corecomp = str_replace(dataelement, ".*results ", "") %>% str_trim()
        ) %>% 
      separate(corecomp, c("age", "sex", "resultsstatus"), sep = ",")  

# Reorder -----------------------------------------------------------------

  #reorder and remove dataelement variables
    fvstyle <- fvstyle %>%
      select(operatingunit, snu1, psnu, sitename, disaggregate, indicator, numeratordenom, 
             indicatortype, disaggregate, age, sex, resultstatus, modality, fy2018q2)

# Export ------------------------------------------------------------------

  #export as csv
    write_csv(fvstyle, file.path(folderpath, "fy18q2template-hts_index.csv"), na = "")
      