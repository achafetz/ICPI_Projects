
#import data
  fvdata <- read_delim(file.path(datafv, "ICPI_FactView_OU_IM_20170702_v2_1.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)

# change all header names to lower case to make it easier to use
  names(fvdata) <- tolower(names(fvdata))


#how many unqiue mechanism are there (non-dedups)?
  fvdata %>% filter(!mechanismid %in% c(0,1))  %>% count(mechanismid)


#which mechanism 
dups <- fvdata %>%
  filter(!mechanismid %in% c(0,1))  %>% 
  select(operatingunit, mechanismid, primepartner, implementingmechanismname) %>%
  distinct(.) %>%
  group_by(mechanismid) %>%
  mutate(cnt = n()) %>%
  filter(cnt>1)


#distinct list of mech ids 
  dup_list <- fvdata %>%
    filter(!mechanismid %in% c(0,1))  %>% 
    select(operatingunit, mechanismid, primepartner, implementingmechanismname) %>%
    distinct(.) %>%
    group_by(mechanismid) %>%
    mutate(cnt = n()) %>%
    filter(cnt>1) %>%
    select(mechanismid, cnt) %>%
    distinct()

#merge back onto fv
  fv_dups <- inner_join(fvdata, dup_list, by="mechanismid") %>%
    mutate(fy2017cum = fy2017q1 + fy2017q2) %>%
    group_by(operatingunit, mechanismid, primepartner, implementingmechanismname) %>%
    summarize_at(vars(fy2015apr, fy2016_targets, fy2016apr, fy2017_targets, fy2017cum), funs(sum(., na.rm=TRUE))) %>%
    mutate(fy2015apr = ifelse(fy2015apr >0, 1, NA)) %>%
    mutate(fy2016_targets = ifelse(fy2016_targets >0, 1, NA)) %>% 
    mutate(fy2016apr = ifelse(fy2016apr >0, 1, NA)) %>%
    mutate(fy2017_targets = ifelse(fy2017_targets >0, 1, NA)) %>% 
    mutate(fy2017cum = ifelse(fy2017cum >0, 1, NA)) 
  
  write.csv2(fv_dups, file="fv_mech_duplicates.csv", na="", sep=",")
  
  write.csv(fv_dups, file = "fv_mech_duplicates.csv")
    
    mutate_at(fv_dups, vars(fy2015apr:fy2017cum), . = ifelse(. > 0, 1, NA), )
    
    
    
      

