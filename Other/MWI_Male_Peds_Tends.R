## Project: Trends in Males and Chidlren
## Author:  A.Chafetz
## Purpose: create trends for men and children for B.Smith, establish benchmarks
## Date:    2018-12-11
## Updated: 2018-12-12

# NOTE: Age/Sex disaggs for HTS_TST/TX_NEW are only available in the 5 
#         accelerantion districts since those have EMRs 


# SETUP -------------------------------------------------------------------

  #dependencies
    library(tidyverse)
    library(scales)
    library(knitr)
    library(kableExtra)
  
  #import data
    df_mwi <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1.rds") %>% 
     filter(operatingunit == "Malawi")


# MUNGE -------------------------------------------------------------------

  #identify the 5 acceleration districts (EMRs)
    acceleration <- c("Blantyre District",
                      "Zomba District",
                      "Mangochi District",
                      "Machinga District",
                      "Chikwawa District")
  #subset to focus
    df_mwi <- df_mwi %>% 
      filter(indicator == "HTS_TST_POS",
             fundingagency == "USAID",
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             psnu %in% acceleration)
    

# OVERALL ACHIEVEMENT -----------------------------------------------------
  # NOTE: EQUIP gave Baylor TSP targets, but no data on how they were 
  #         distributed at a PSNU level, so achievement will in fact
  #         be better for EQUIP nationally but not relected here
    
  #setup data
    ach <- df_mwi %>% 
      group_by(implementingmechanismname) %>% 
      summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
      filter_at(vars(fy2018apr, fy2018_targets), any_vars(. != 0)) %>% 
      mutate(fy2018ach = (round(fy2018apr / fy2018_targets, 2) *100),
             fy2018ach = ifelse(is.infinite(fy2018ach), 0, fy2018ach)) %>% 
      arrange(desc(fy2018ach))
    
  #knit table
    ach %>% 
      rename(` ` = implementingmechanismname, 
             `FY18 APR` = fy2018apr,
             `FY18 Targets` = fy2018_targets,
             `Achievement (%)` = fy2018ach) %>%
      kable(format.args = list(big.mark = ",", zero.print = FALSE), align = "r", 
            caption = "Malawi FY18 Achievement in Acceleration Districts") %>% 
      kable_styling()



# VISUALIZE TRENDS --------------------------------------------------------

  #structure trend data
    trend <- df_mwi %>% 
      filter(!agecoarse %in% c(NA, "Unknown Age")) %>% 
      mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>% 
      select(-fy2017q1) %>% 
      group_by(implementingmechanismname, sex) %>% 
      summarise_at(vars(fy2019_targets, contains("2018q")), sum, na.rm = TRUE) %>%
      ungroup() %>% 
      filter_at(vars(fy2019_targets, contains("2018q")), any_vars(. != 0))
    
  #reshape long for graphing 
    trend_lng <- trend %>%
      filter_at(vars(contains("2018q")), any_vars(. != 0)) %>% 
      select(-fy2019_targets) %>% 
      gather(pd, val, -implementingmechanismname, -sex) %>% 
      mutate(pd = str_remove(pd, "20") %>% toupper(.),
             val = ifelse(val == 0, NA, val))
    
  #function for plots
    plot_trend <- function(df, grp){
      
      
      main <- paste("Malawi", ifelse(grp == "Male", "Men", "Children"), "Testing Positive Trends in Acceleration Districts")
      
      df <- df %>% 
        filter(sex == grp)
      
      order <- df %>% 
        filter(pd == "FY18Q4") %>% 
        arrange(desc(val)) %>% 
        pull(implementingmechanismname)
      
      df <- df %>% 
        mutate(implementingmechanismname = factor(implementingmechanismname, levels = order))
      
      df %>% 
        ggplot(aes(pd, val, group = implementingmechanismname, color = implementingmechanismname)) +
        geom_path(size = 2, na.rm = TRUE) +
        geom_point(size = 6, na.rm = TRUE) +
        scale_y_continuous(label = scales::comma) + 
        scale_x_discrete(label = c("Q1", "Q2", "Q3", "Q4")) +
        labs(title = main,
             subtitle = "FY18 HTS_TST_POS",
             x = "", y = "",
             caption = "Source: PEPFAR MSD FY18Q4i") +
        facet_grid(. ~ implementingmechanismname) +
        theme(legend.position = "none")
    }
  
  #visualize for men and children
    map(.x = c("Male", "Children"),
        .f = ~ plot_trend(trend_lng, .x))
      
  #function for table
    tabulate_trend <- function(df, grp){
      
      df <- df %>% 
        filter(sex == grp) %>% 
        select(-sex)
      
      cap <- paste("Malawi FY18 HTS_TST_POS", ifelse(grp == "Male", "Men", "Children <15"), "in Acceleration Districts")
      df %>% 
        rename_all(~toupper(.)) %>% 
        rename(` ` = IMPLEMENTINGMECHANISMNAME) %>%
        arrange(desc(FY2018Q4)) %>% 
        kable(format.args = list(big.mark = ",", zero.print = FALSE), align = "r", 
              caption = cap) %>% 
        kable_styling()
    }
    
  #generate trend tables for men and children
    map(.x = c("Male", "Children"),
        .f = ~ tabulate_trend(trend_lng, .x))
