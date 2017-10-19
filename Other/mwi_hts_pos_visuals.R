##  MWI HTS_POS Reporting Issue 
##  A.Chafetz, USAID
##  Purpose: visualize FY17 reporting issue for HTS_POS
##  Created: 10/18/17 


#import
  df_mer  <- read_tsv("~/ICPI/Data/ICPI_FactView_PSNU_20170922_v2_1.txt")
  names(df_mer) <- tolower(names(df_mer))

#clean up
  df_mwi <- df_mer %>%
    filter(operatingunit == "Malawi", indicator %in% c("HTS_TST", "HTS_TST_POS"), standardizeddisaggregate == "Total Numerator") %>%
    select(-fy2015q2, -contains("targets"), -contains("apr")) %>% 
    group_by(operatingunit, indicator) %>% 
    summarize_at(vars(fy2015q3:fy2017q3), funs(sum(., na.rm=TRUE))) %>%
    ungroup %>%
    gather(pd, values, fy2015q3:fy2017q3) %>%
    filter(values !=0) %>%
    separate(pd, c("fy", "qtr"), -3, remove = FALSE)

#visuals 
  
  #HTS + HTS_POS
  df_mwi %>%
    ggplot()  + 
      geom_col(mapping = aes(x = pd, y = values, group = indicator)) +
      facet_grid(indicator ~ ., scales = "free_y") +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_discrete(labels = c("FY15Q3", "Q4", "FY16Q1","Q2", "Q3", "Q4","FY17YQ1", "Q2", "Q3")) + 
      labs(title = "PEPFAR Reported Testing in Malawi", x = "", y = "", caption = "Source: ICPI Fact View FY17Q3v2.1")
    
  #HTS_POS
  df_mwi %>%
    filter(indicator== "HTS_TST_POS", fy!="fy2015") %>%
    ggplot()  + 
      geom_col(mapping = aes(x = qtr, y = values, group = fy)) +
      facet_wrap(~fy) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Issue around FY17 HTS_TST_POS Reporting in Malawi", x = "Quarter", y = "HTS_TST_POS (Total Num)", 
        caption = "Source: ICPI Fact View FY17Q3v2.1")

  #HTS Positivity
  df_mwi %>% 
    spread(indicator, values) %>%
    mutate(HTS_TST_YIELD = HTS_TST_POS/HTS_TST) %>%
    gather(indicator, values, HTS_TST, HTS_TST_POS,HTS_TST_YIELD) %>%
    filter(indicator== "HTS_TST_YIELD") %>%
    ggplot()  + 
      geom_line(mapping = aes(x = pd, y = values, group=1)) +
      geom_point(mapping = aes(x = pd, y = values)) +
      scale_y_continuous(labels = scales::percent) + 
      scale_x_discrete(labels = c("FY15Q3", "Q4", "FY16Q1","Q2", "Q3", "Q4","FY17YQ1", "Q2", "Q3")) + 
      labs(title = "PEPFAR Testing Positivity in Malawi", x = "Period", y = "Positivity (HTS_POS/HTS_TST)", caption = "Source: ICPI Fact View FY17Q3v2.1")

  
  