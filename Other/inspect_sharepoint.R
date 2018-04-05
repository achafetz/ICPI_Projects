##   INSPECT SHAREPOINT USAGE REPORT
##   A.Chafetz, USAID
##   Purpose: explore tool "usage"
##   Date: 2018-04-04
##   Updated: 2018-04-05

## Notes
#  - reports generated quarterly
#  - files downloaded from PEPFAR Sharepoint
#     https://www.pepfar.net/OGAC-HQ/icpi/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2FOGAC-HQ%2Ficpi%2FShared%20Documents%2FClusters%2FOCM%20Team%2FSharePoint%20Metrics&View={94C838B2-E166-4122-B8B4-7BEB9E1BC12B}&InitialTabId=Ribbon%2ELibrary&VisibilityContext=WSSTabPersistence


# Dependencies ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(lubridate)


# Import and Munge --------------------------------------------------------

#import
  sharepoint <- read_excel("~/tmp/ICPI Usage Metrics Report FY18Q1 20171212.xlsx", 
                       sheet = "Report Data 1", skip = 2, 
                       col_types = c("text", "text", "text", "text", "text", "date",
                                     "text", "text", "text", "text", "text", "text"))
  #skimr::skim(sharepoint)
  #glimpse(sharepoint)
  
#adjust varable names, removing "bad" characters and converting to lower case
  names(sharepoint) <- gsub("\\(|\\)", "", names(sharepoint))
  names(sharepoint) <- gsub(" ", "_", names(sharepoint))
  names(sharepoint) <- tolower(names(sharepoint))

#remove completely blank/meaningless columns
  sharepoint <- sharepoint %>% 
    select(-site_id, -item_id, -custom_event_name, -event_data, -event_source, -source_name, -app_id) 

#split out name and id
  sharepoint <- sharepoint %>% 
      separate(user_id, c("user", "user_id"), sep = "<") %>% 
      mutate(user_id = str_replace(user_id, "i:0#.w\\|pepfar\\\\", "")  %>% str_replace(">", ""))

#keep only documents (remove List and Folder) and only care about views - distinct(sharepoint, event)
  sharepoint <- sharepoint %>% 
      filter(item_type == "Document", event == "View") %>% 
      select(-item_type, -event)
  
#extract document name from document_location
  sharepoint <- sharepoint %>% 
      mutate(document = str_extract(document_location, "/[^/]*$") %>% str_replace("/", ""),
             file_type = str_extract(document, "\\.[^\\.]*$") %>% str_replace("\\.", "") %>% tolower()
          ) %>% 
      select(-document_location) 
  
#convert time to ESD & add month/weekday
  sharepoint <- sharepoint %>% 
      mutate(datetime = ymd_hms(occurred_gmt, tz = "GMT") %>% with_tz("America/New_York")) %>% 
      select(-occurred_gmt) %>% 
      mutate(month = month(datetime, label = TRUE),
             day = wday(datetime, label = TRUE)
      ) 
    
# Inspect -----------------------------------------------------------------
  
#how many distinct users were there?
  n_distinct(sharepoint$user_id)
  
#how many document were there?
  n_distinct(sharepoint$document) 
  
#how many total downloads per user (max 1 per tool)
  sharepoint %>% 
    distinct(user, document) %>% 
    count(user) %>% 
    arrange(desc(n))
     
#how many total downloads per user (max 1 per tool)
   sharepoint %>% 
     distinct(user, document) %>% 
     count(document) %>% 
     arrange(desc(n)) 
   
#how many total downloads per user per month (max 1 per tool)
   sharepoint %>% 
       distinct(user, document, month) %>% 
       count(month)    


   
#any way to distinguish between downloads and uploads? 
#why so many views within seconds of the same document?
     