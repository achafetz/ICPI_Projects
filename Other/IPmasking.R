# Mask Mechanism Names, Partners, Numbers
# A.Chafetz, USAID
# Purpose: deidentify mechanisms
# Updated: 9/29/17

#requirements
  library("readr")
  library("tidyverse")
  library("stringi")

#factview folder location
  datafv <- "C:/Users/achafetz/Documents/ICPI/Data"

#import
  fvdata <- read_delim(file.path(datafv, paste("ICPI_FactView_OU_IM_20170815_v1_1.txt", sep="")), "\t", escape_double = FALSE, trim_ws = TRUE)
  names(fvdata) <- tolower(names(fvdata)) #change headers to all lower case

#create list of unique mechanisms ids, names, and partners
  df_unique <- fvdata %>%
    filter(mechanismid > 1) %>%  #remove dedups 
    mutate(num = 1) %>% #give all lines a number to sum (otherwise could miss mechs for non-summed year)
    group_by(operatingunit, mechanismid, implementingmechanismname, primepartner) %>%
    summarize_at(vars(num), funs(sum(., na.rm=TRUE))) %>% #aggregate
    ungroup %>%
    select(-num) %>%  #remove num

#new, masked mechanism id
  mutate(mechanismid_new = row_number() + 12345)

#new,masked prime partner
  #unique list of prime partners
  df_unique_pp <- fvdata %>%
    filter(mechanismid > 1) %>%  #remove dedups 
    mutate(num = 1) %>% #give all lines a number to sum (otherwise could miss mechs for non-summed year)
    group_by(primepartner) %>% #group = prime partners
    summarize_at(vars(num), funs(sum(., na.rm=TRUE))) %>% #aggregate
    ungroup %>%
    select(-num) #remove num

  #create random 5 letter string
    n <- nrow(df_unique_pp) #fine number of row to create a new partner name for
    primepartner_new <- stri_rand_strings(`n`, 5, '[A-Z]') #create vector of partner names with 5 random string
    
  #convert new partner name to a df
    df_temp <- data.frame(primepartner_new)
    
  #join the real names with generated values
    df_unique_pp <- bind_cols(df_unique_pp, df_temp)

  #merge new names to concordance data frame
    df_unique <- left_join(df_unique, df_unique_pp, by = "primepartner")

#export
  write.csv(df_unique, "C:/Users/achafetz/Downloads/IPmasking.csv", na="", row.names = FALSE)


  rm(df_unique_pp, df_temp, primepartner_new, n, datafv, fvdata)

