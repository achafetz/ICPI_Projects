library(tidyverse)

#import
  setwd("C:/Users/achafetz/Documents/ICPI/Data/")
  fvdata <- read_delim("ICPI_FactView_OU_IM_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
  names(fvdata) <- tolower(names(fvdata))

#identify mechanisms with one or multiple rows
  df_ims <- fvdata %>%
    select(operatingunit, mechanismid, primepartner, implementingmechanismname) %>%  #subset to mech info (variables)
    distinct(.) %>%  #subset to unique rows by the variables above 
    filter(mechanismid>1) %>%  #remove dedups
    group_by(mechanismid) %>%
    mutate(n = n())  #create new variable of group counts, ie identify mechanisms with multiple names
  rm(fvdata)
  
#how many unique mechanisms?
  nrow(distinct(df_ims,mechanismid))

#how many unique mechanisms?
  n_max <- max(df_ims$n) #max repeats
  for (i in 1:n_max){
  t <- df_ims %>%
    filter(n==i)  %>%  #remove dedups
    nrow(.)
  print(paste(t, "mechanisms have", i, "observations"))
  }
  rm(n_max, i, t)
  
#distribution by country
  df_opunit <- df_ims %>%
      select(operatingunit, mechanismid, n) %>%
      mutate(match = lag(mechanismid)) %>%
      filter(is.na(match)) %>%
      mutate(n_1 = ifelse(n==1, 1, 0)) %>%
      mutate(n_2 = ifelse(n==2, 1, 0)) %>%
      mutate(n_3 = ifelse(n==3, 1, 0))  %>%
      group_by(operatingunit) %>% 
      summarise_all(funs(sum(.))) %>%
      select(operatingunit, n_1:n_3)

  