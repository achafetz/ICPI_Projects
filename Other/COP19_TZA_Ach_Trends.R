# PROJECT:  COP19 
# AUTHOR:   A.CHAFETZ
# PURPOSE:  Create trend visuals for partners in HTS_POS and TX_NEW
# DATE:     2019-04-16


#dependencies
  library(tidyverse)
  library(scales)
  library(extrafont)
  library(genPPR)
  library(ICPIutilities)
  
#import dataset
  df_mer <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-19_20190322_v2_1.rds")

#FY19Q1i fix for HTS_TST_POS targets
  
  # df_mer <- df_mer %>% 
  #   add_pos("HTS_TST") 
  # 
#filter to variable of use/need
  df_tza <- df_mer %>% 
    filter(operatingunit == "Tanzania",
           fundingagency != "Dedup",
           indicator %in% c("HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW"), 
           standardizeddisaggregate == "Total Numerator") %>% 
    mutate(mechanismid = case_when(mechanismid == "18627" ~ "16763",
                                   TRUE ~ mechanismid)) %>% 
    rename_official() #clean mech/partner names

#summ
  df_tza <- df_tza %>% 
    group_by(fundingagency, snu1, indicator, mechanismid, primepartner) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup() 
  
#add in net new targets
  df_tza_old <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY15-16_20190322_v2_1_Tanzania.rds")
  
  df_tza_old <- df_tza_old %>% 
    filter(indicator == "TX_CURR",
           fundingagency != "Dedup",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(snu1, mechanismid) %>% 
    summarise_at(vars(fy2016q4), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(fy2016q4 > 0)
  
  df_nn <- df_tza %>% 
    filter(indicator == "TX_CURR") %>% 
    select(snu1, mechanismid, ends_with("q4"), ends_with("targets")) %>% 
    right_join(df_tza_old, ., by = c("snu1","mechanismid")) %>% 
    mutate(fy2017_targets_nn = ifelse(fy2017_targets == 0, 0, fy2017_targets - fy2016q4),
           fy2018_targets_nn = ifelse(fy2018_targets == 0, 0, fy2018_targets - fy2017q4),
           fy2019_targets_nn = ifelse(fy2019_targets == 0, 0, fy2019_targets - fy2018q4),
           indicator = "TX_NET_NEW") %>% 
    select(snu1, mechanismid, indicator, ends_with("_nn")) %>% 
    mutate_at(vars(ends_with("_nn")), ~ ifelse(. < 0, 0, .))
  
  df_tza <- df_tza %>% 
    filter(indicator != "TX_CURR") %>% 
    left_join(df_nn, by = c("snu1", "mechanismid", "indicator")) %>%
    mutate(fy2017_targets = ifelse(indicator == "TX_NET_NEW" & fy2017_targets_nn > 0, fy2017_targets_nn, fy2017_targets),
           fy2018_targets = ifelse(indicator == "TX_NET_NEW" & fy2018_targets_nn > 0, fy2018_targets_nn, fy2018_targets),
           fy2019_targets = ifelse(indicator == "TX_NET_NEW" & fy2019_targets_nn > 0, fy2019_targets_nn, fy2019_targets)) %>% 
    select(-ends_with("_nn"))
    
#add missing SNU1 from add_pos
  # df_tza <- df_tza %>% 
  #   group_by(psnu) %>% 
  #   fill(snu1) %>% 
  #   ungroup() 
  
  # rm(df_mer)
  
#identify SNU ordering (HTS_TST_POS FY19 Targets)
  
  snu_order <- df_tza %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    count(snu1, wt = fy2019_targets) %>% 
    arrange(n) %>% 
    pull(snu1)
  
#function for reshaping dataset long
  go_long <- function(df, elem){
    
    var <- ifelse(elem == "q", "result", "target")
    
    df <- df %>%
      group_by(snu1, indicator, mechanismid, primepartner) %>% 
      summarise_at(vars(contains(elem)), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      gather(pd, !!var, contains(elem)) %>% 
      separate(pd, c("fy", "qtr"), sep = 6)
    
    if(elem == "targets")
      df <- select(df, -qtr)
    
    return(df)
  }

#create long results dataset and bing targetson
  df_results <- go_long(df_tza, "q")
  df_targets <- go_long(df_tza, "targets")

  
#bind df together (targets will repeat) & create achievement
  df_ach <- df_results %>% 
    left_join(df_targets, by = c("snu1", "indicator", "mechanismid", "primepartner", "fy")) %>% 
    arrange(primepartner, snu1, indicator, fy, qtr) %>% 
    rename(target_ann = target) %>% 
    group_by(mechanismid, primepartner, snu1, indicator, fy) %>%
    mutate(result_cum = cumsum(result) ,
           ach = result_cum / target_ann) %>% 
    ungroup() %>% 
    select(-ach, -target_ann, everything())
  
  rm(df_results, df_targets)
  
#adjustments for graphing
  df_ach <- df_ach %>% 
    unite(pd, c("fy", "qtr"), sep = "", remove = FALSE) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(),
           grade = case_when(target_ann == 0        ~ as.character(NA),
                            qtr== "q1" & ach < .01 ~ "low",
                            qtr== "q2" & ach < .25 ~ "low",
                            qtr== "q3" & ach < .50 ~ "low",
                            qtr== "q4" & ach < .75 ~ "low",
                            qtr== "q1" & ach < .15 ~ "med",
                            qtr== "q2" & ach < .40 ~ "med",
                            qtr== "q3" & ach < .65 ~ "med",
                            qtr== "q4" & ach < .90 ~ "med",
                            TRUE                   ~ "okay"),
           grade = factor(grade, c("low", "med", "okay")),
           score = case_when(grade == "low" ~ 2,
                             grade == "med" ~ 1,
                             TRUE           ~ 0),
           font_color = case_when(grade == "okay" ~ "#595959", 
                                  target_ann == 0 & result > 0 ~ "#595959",
                                  TRUE ~ "white"))

  
#plot function
  plot_achtrend <- function(df, mechid, filepath_save = NULL){
    
    partner <- df %>% 
      filter(mechanismid == mechid) %>% 
      distinct(primepartner) %>% 
      pull(primepartner)
    
    partner_title <- paste(partner, "Trends")
    
    #create df with full set of options so all parnter have the same grid
    fulloptions <- 
      crossing(snu1 = unique(df$snu1), 
               pd = unique(df$pd),
               indicator= unique(df$indicator)) %>% 
      separate(pd, c("fy", "qtr"), sep = 4, remove = FALSE) %>% 
      mutate_at(vars(fy, qtr), ~ tolower(.)) %>% 
      mutate(fy = str_replace(fy, "fy", "fy20"))
    
    df_plot <- df %>% 
      filter(mechanismid == mechid) %>% 
      #mutate(snu1 = fct_reorder(snu1, score, sum))
      left_join(fulloptions, ., 
                by = c("snu1", "pd", "fy", "qtr", "indicator")) %>% 
      mutate(snu1 = factor(snu1, snu_order),
             indicator = factor(indicator, c("HTS_TST_POS", "TX_NEW", "TX_NET_NEW")))
      
    plot <- df_plot %>%
      ggplot(aes(pd, snu1, fill = grade)) +
      geom_tile(color = "white") +
      geom_vline(xintercept = c(4.5, 8.5), color = "white", size = 3) +
      geom_text(aes(label = comma(result)), color = df_plot$font_color,
                family = "Gill Sans MT", size = 2) +
      scale_x_discrete(labels = c("FY17", rep("", 3), "FY18", rep("", 3), "FY19")) +
      scale_fill_manual(values = c("#CC5234", "#d9812c", "#c4d9d1"),
                        labels = c(" poor ", " low ", " okay ", " no/negative targets"),
                        drop = FALSE,
                        na.translate = TRUE) +
      labs(x = "", y = "",
           title = partner_title,
           subtitle = "FY17-19 quarterly result labeled; color denotes cumulative performance against annual targets.",
           caption = "Note: Regions sorted on FY19 HTS_POS targets
           Source: FY19Q1c MSD") +
      facet_wrap(. ~ indicator) +
      theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 10),
            axis.text.y = element_text(size = 8),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 13, color = "#595959"),
            panel.grid = element_blank(), #element_line(color = "#ebebeb"),
            plot.title = element_text(size = 14, face = "bold", color = "black"),
            plot.caption = element_text(size = 9,  color = "#595959")
      )
    
    if(!is.null(filepath_save)){
      ggsave(paste0("v_", format(Sys.time(), "%H%M%OS1"), "_", mechid, ".png"), #paste0("COP19_TZA_Ach_Trends_", mechid, ".png"),
             path = filepath_save,
             height = 5.5, width = 9.5, units = "in", dpi = 300)
      print(paste(mechid, "...saved"))
    }

    
    return(plot)
  }

  
#define mechanism ordering
  
  key_mechs <- tibble::tribble(
    ~fundingagency, ~mechanismid,                                                                 ~primepartner,
           "USAID",        "18060",                                  "Elizabeth Glaser Pediatric AIDS Foundation",
           "USAID",        "16784",                                                                     "JHPIEGO",
           "USAID",        "18237",                                                 "Deloitte Consulting Limited",
           "USAID",        "16787",                                                         "John Snow Inc (JSI)",
           "USAID",        "17103", "Baylor College of Medicine International Pediatric AIDS Initiative/Tanzania",
         "HHS/CDC",        "17986",                           "Ariel Glaser Pediatric AIDS Healthcare Initiative",
         "HHS/CDC",        "80095",                                           "Management development for Health",
         "HHS/CDC",        "17991",                                                         "Columbia University",
             "DOD",        "16763",                                                    "Henry Jackson Foundation"
    ) %>% 
    pull(mechanismid)

  mech_list <- df_tza  %>% 
    filter(indicator == "HTS_TST_POS",
           mechanismid %in% key_mechs) %>% 
    group_by(fundingagency, mechanismid, primepartner, indicator) %>% 
    summarise_at(vars(fy2017apr, fy2018apr, fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(fundingagency), desc(fy2019_targets)) %>% 
    pull(mechanismid)
  

#produce visuals
  walk(.x = mech_list,
       .f = ~ plot_achtrend(df_ach, .x, "../Downloads"))


  plot_achtrend(df_ach, "17103")

