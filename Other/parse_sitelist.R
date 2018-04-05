##   Parse Site Info
##   Aaron Chafetz
##   Purpose: take site info in one cell and parse it into distinct columns
##   Date: 2018.03.14

#library
  pacman::p_load(tidyverse, readxl)
  
#where is the file located? and what file?
folderpath <- "C:/Users/achafetz/Downloads/"
file <- "COP18 (FY19) South Sudan Prioritization_Updated_2018.03.14.xlsx"

#import
sites <- read_excel(file.path(folderpath, file),
                    sheet = "NORMAL")
#add an NA where facility/community is for not yet distributed rows
sites <- sites %>% 
  mutate(siteID = str_replace(siteID, "NOT YET DISTRIBUTED", "NOT YET DISTRIBUTED {NA}"))

#replace end brackets with blanks and then replace opening brackets with common delimitor ">"
sites <- sites %>% 
  mutate(siteID = str_replace_all(siteID, "[})]", ""),
         siteID = str_replace_all(siteID, "[{())]", "> ")) %>% 
#parse into distinct columns
  separate(siteID, c("psnu", "site_name", "site_type", "site_uid"), sep = ">") %>% 
  filter(!is.na(site_uid)) %>% 
#remove leading/trailing spaces
  mutate_all(~ trimws(.))
#export
  write_csv(sites, file.path(folderpath, "SSD_parsed_site_list.csv"), na = "")


