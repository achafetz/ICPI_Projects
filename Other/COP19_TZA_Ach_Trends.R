# PROJECT:  COP19 
# AUTHOR:   A.CHAFETZ
# PURPOSE:  Create trend visuals for partners in HTS_POS and TX_NEW
# DATE:     2019-03-08


#dependencies
  library(tidyverse)
  library(scales)
  library(extrafont)
  library(genPPR)
  library(ICPIutilities)

#import dataset
  df_mer <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-19_20190215_v1_1.rds")

#FY19Q1i fix for HTS_TST_POS targets
  
  df_mer <- df_mer %>% 
    add_pos("HTS_TST") 
  
#filter to variable of use/need
  df_tza <- df_mer %>% 
    filter(operatingunit == "Tanzania",
           fundingagency != "Dedup",
           indicator %in% c("HTS_TST_POS", "TX_NEW"), 
           standardizeddisaggregate == "Total Numerator") %>% 
    rename_official() #clean mech/partner names

  # add missing SNU1 from add_pos
  df_tza <- df_tza %>% 
    group_by(psnu) %>% 
    fill(snu1) %>% 
    ungroup() 
  
  rm(df_mer)
  
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
      mutate(snu1 = factor(snu1, snu_order))
      
    plot <- df_plot %>%
      ggplot(aes(pd, snu1, fill = grade)) +
      geom_tile(color = "white") +
      geom_vline(xintercept = c(4.5, 8.5), color = "white", size = 3) +
      geom_text(aes(label = comma(result)), color = df_plot$font_color,
                family = "Gill Sans MT", size = 2) +
      scale_x_discrete(labels = c("FY17", rep("", 3), "FY18", rep("", 3), "FY19")) +
      scale_fill_manual(values = c("#CC5234", "#d9812c", "#c4d9d1"),
                        labels = c("poor", "low", "okay", "no targets set"),
                        drop = FALSE,
                        na.translate = TRUE) +
      labs(x = "", y = "",
           title = partner_title,
           subtitle = "FY17-19 quarterly result labeled; color denotes cumulative performance against annual targets.",
           caption = "Note: Regions sorted on FY19 HTS_POS targets
           Source: FY19Q1i MSD") +
      facet_wrap(. ~ indicator) +
      theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 14, color = "#595959"),
            panel.grid = element_blank(), #element_line(color = "#ebebeb"),
            plot.title = element_text(size = 15, face = "bold", color = "black"),
            plot.caption = element_text(size = 11,  color = "#595959")
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
  mech_list <- df_tza  %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    group_by(fundingagency, mechanismid, primepartner, indicator) %>% 
    summarise_at(vars(fy2017apr, fy2018apr, fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(fundingagency), desc(fy2019_targets)) %>% 
    pull(mechanismid)

#produce visuals
  walk(.x = mech_list,
       .f = ~ plot_achtrend(df_ach, .x, "../Downloads"))




