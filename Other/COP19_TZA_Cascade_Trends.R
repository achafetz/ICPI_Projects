# PROJECT:  COP19 TZA
# AUTHOR:   A.CHAFETZ
# PURPOSE:  Create trend visuals for cascade indicators
# DATE:     2019-04-17

#dependencies
  library(tidyverse)
  library(readxl)
  library(scales)
  library(lubridate)
  library(extrafont)
  library(gridExtra)
  library(grid)

#import monthly data
  filepath <- "../Downloads/All Monthly Raw Data in given timeframe.xlsx"
  df_monthly <- read_excel(filepath)

#tidy dataset
  df_monthly <- df_monthly %>% 
    gather(ind_monthly, val, -Partner:-Month, na.rm = TRUE) %>% 
    filter(val != 0) %>% 
    rename(orgunituid = `DATIM ID`) %>% 
    rename_all(tolower)

#mapping table from key monthly reporting indicators to MER-ish names
  key_ind <- tibble::tribble(
                                                                  ~ind_monthly,      ~indicator,     ~sex,  ~age,
                                         "Males tested at site, 15+ years old",       "HTS_TST",   "Male", "15+",
                                       "Females tested at site, 15+ years old",       "HTS_TST", "Female", "15+",
                                      "Children tested at site, <15 years old",       "HTS_TST",   "Peds", "<15",
                                     "Males tested POS at site, 15+ years old",   "HTS_TST_POS",   "Male", "15+",
                                   "Females tested POS at site, 15+ years old",   "HTS_TST_POS", "Female", "15+",
                                  "Children tested POS at site, <15 years old",   "HTS_TST_POS",   "Peds", "<15",
                                                  "Male TX_NEW, 15+ years old",        "TX_NEW",   "Male", "15+",
                                                "Female TX_NEW, 15+ years old",        "TX_NEW", "Female", "15+",
                                                 "Child TX_NEW, <15 years old",        "TX_NEW",   "Peds", "<15",
                 "Total on Treatment at Facility at the end of previous month", "TX_CURR_prior",       NA,    NA,
               "Total on Treatment at Facility at the end of reporting period",       "TX_CURR",       NA,    NA
               )

#joing mapping to monthly data, dropping all non-essential indicators
  df_monthly <- inner_join(df_monthly, key_ind, by = "ind_monthly")

  rm(key_ind)

#aggregate up to indicator level
  df_monthly <- df_monthly %>% 
    select(-c(ind_monthly, sex, age)) %>% 
    group_by_if(is.character) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup()
  
#clean up date, conver to R date
  df_monthly <- df_monthly %>% 
    mutate(month = str_replace(month, " ", " 1, "),
           month = mdy(month))
  
#visualize cascade
  # df_monthly %>%
  #   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW")) %>%
  #   ggplot(aes(month, val)) +
  #   geom_col() +
  #   scale_y_continuous(labels = comma) +
  #   facet_grid(indicator ~ ., scales = "free_y")


#create positivity, linkage and retention metrics (at partner level)
  df_monthly_pct <- df_monthly %>% 
    group_by(partner, agency, month, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>%
    mutate(Positivity = HTS_TST_POS/HTS_TST,
           Linkage = TX_NEW/HTS_TST_POS,
           Retention = TX_CURR/(TX_CURR_prior + TX_NEW),
           partner = str_replace(partner, "\\/", "-"))
  
#keep HTS_TST as raw values
  df_monthly_hts <- df_monthly_pct %>% 
    select(partner:HTS_TST) %>%
    gather(indicator, val, HTS_TST) %>% 
    mutate(pd = quarter(month, with_year = TRUE, fiscal_start = 10) %>% as.character()) %>% 
    group_by(partner, indicator) %>% 
    mutate(max = max(val)*1.1) %>% #used to keep labels within bounds
    ungroup()  

#munge dataset for pct graphing
  df_monthly_pct <- df_monthly_pct %>% 
    select(-HTS_TST:-TX_NEW) %>% 
    gather(indicator, val, Positivity:Retention) %>% 
    mutate(indicator = factor(indicator, c("Positivity", "Linkage", "Retention")),
           pd = quarter(month, with_year = TRUE, fiscal_start = 10) %>% as.character()) %>% 
    group_by(partner, indicator) %>% 
    mutate(max = max(val) + ifelse(max(val) < .15, .05, .15)) %>% #used to keep labels within bounds
    ungroup() %>% 
    mutate(val = ifelse(val == 0, NA, val))


plot_cascade <- function(prime, filepath_save = NULL){
  
  #graph title
    partner_title <- paste(prime, "Trends")
  
  #grah HTS bar chart
    viz_hts <- df_monthly_hts %>% 
      filter(partner == prime) %>% 
      ggplot(aes(month, val, fill = pd)) +
      geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
      geom_col() +
      geom_point(aes(y = max), color = "white", size = 0) +
      geom_text(aes(label = comma(val)),
                family = "Gill Sans MT",
                vjust = -1,
                color = "#595959") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#3498db", "#1abc9c")) +
      expand_limits(y = 0) +
      facet_grid(. ~indicator, scales = "free_y") +
      labs(x = "", y = "") +
      theme_light() +
      theme(legend.position = "none",
            panel.border = element_blank(),
            text = element_text(family = "Gill Sans MT", color = "#595959"),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold")
            )
    
  #graph positivity/linkage/retention
    viz_pct <- df_monthly_pct %>% 
      filter(partner == prime) %>% 
      ggplot(aes(month, val, group = pd, color = pd)) +
      geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
      geom_point(aes(y = max), color = "white", size = 0) +
      geom_line(size = 1) +
      geom_point(size = 4) +
      geom_text(aes(label = percent(val)),
                family = "Gill Sans MT",
                vjust = -1,
                color = "#595959") +
      expand_limits(y = 0) +
      facet_grid(indicator ~ ., scales = "free_y", switch = "y") +
      scale_y_continuous(labels = percent_format(1)) +
      scale_color_manual(values = c("#3498db", "#1abc9c")) +
      labs(x = "", y = "") +
      theme_light() +
      theme(legend.position = "none",
            panel.border = element_blank(),
            text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold"))
    
  #combine
    plot <- arrangeGrob(viz_hts, viz_pct, ncol = 2, 
                 top = textGrob(partner_title, 
                                gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
                                hjust = 1,
                                x = .15),
                 bottom = textGrob(
                                 "Source: Tanzania Monthly Reporting Portal, Basic Raw Data, Oct 2018 - Feb 2019",
                                 gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
                                 hjust = 1,
                                 x = 1
                 ))
    # plot2 <- grid.arrange(viz_hts, viz_pct, ncol = 2, 
    #                     top = textGrob(partner_title, 
    #                                    gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
    #                                    hjust = 1,
    #                                    x = .15),
    #                     bottom = textGrob(
    #                       "Source: Tanzania Monthly Reporting Portal, Basic Raw Data, Oct 2018 - Feb 2019",
    #                       gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
    #                       hjust = 1,
    #                       x = 1
    #                     ))
    
    if(!is.null(filepath_save)){
      ggsave(paste0("COP19_TZA_Cascade_v_", format(Sys.time(), "%H%M%OS1"), "_", prime, ".png"), 
             plot,
             path = filepath_save,
             height = 5.5, width = 9.5, units = "in", dpi = 300)
      print(paste(prime, "...saved"))
    }
    
    
    #return(plot2)
}  

plot_cascade("EGPAF")
plot_cascade("EGPAF", "../Downloads")


partners <- c("Jhpiego-Sauti", "Deloitte", "EGPAF", "Baylor", "JSI-AIDSFree", #USAID
              "AGPAHI", "MDH", "ICAP", #CDC
              "HJF" #DOD
              )
map(.x = partners, 
    .f = ~ plot_cascade(.x, "../Downloads"))


