library(tidyverse)
library(ICPIutilities)
library(scales)
library(gridExtra)
library(extrafont)

# pull 2019-05.22 09:25 by A.Chafetz

#import
path <- "../Downloads/PEPFAR-Data-Genie-OUByIMs-2019-05-22.zip"
df <- read_msd(path, save_rds = FALSE)

df <- df %>%
  select(-contains("approvallevel")) %>% 
  mutate_at(vars(targets:cumulative), as.double) %>% 
  mutate(fiscal_year = as.integer(fiscal_year)) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator))


df <- df %>% reshape_msd("long")


# ADULT INDEX TESTING -----------------------------------------------------

  #index dataset
    df_index_yield <- df %>% 
      filter(indicator == "HTS_TST",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
             trendscoarse != "Unknown Age",
             str_detect(period, "q")) %>%
      mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "Index", "Other")) %>% 
      group_by(period, statushiv, modality, trendscoarse) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      group_by(period, modality, trendscoarse) %>% 
      mutate(yield = val/sum(val)) %>%
      ungroup() %>% 
      mutate(period = str_remove(period, "20") %>% toupper) %>% 
      filter(statushiv == "Positive") %>% 
      group_by(period, trendscoarse) %>% 
      mutate(share = val / sum(val)) %>% 
      ungroup() %>% 
      filter(modality == "Index") %>% 
      gather(indicator, value, val, yield, share) %>% 
      arrange(indicator, period)
  
  #vis yield, adults
    df_index_yield %>% 
      filter(indicator == "yield",
             trendscoarse == "15+") %>% 
      ggplot(aes(period, value, group = indicator, color = indicator)) +
      geom_hline(yintercept = .2, size = 1.5, linetype = "dashed", color = "#595959") +
      geom_line(size = 2) +
      geom_point(size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#16a085") +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "", subtitle = "% Index Positivity (Adults)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))

    ggsave("../Downloads/TZA_index_yield.png", dpi = 300, width = 5.83, height = 4.88, units = "in")
    ggsave("../Downloads/TZA_index_yield2.png", dpi = 300, width = 11.69, height = 5.63, units = "in")
    
    
  #viz positive index tests, adults
    df_index_yield %>% 
      filter(indicator == "val",
             trendscoarse == "15+") %>% 
      ggplot(aes(period, value, fill = indicator)) +
      geom_col(fill = "#2980b9") +
      scale_y_continuous(labels = comma) +
      labs(x = "", y = "", subtitle = "Positive Index Tests (Adults)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(), 
            text = element_text(size = 12))

    ggsave("../Downloads/TZA_index_pos.png", dpi = 300, width = 5.83, height = 2.44, units = "in")
    
  #viz share of positive index tests, adults
    df_index_yield %>% 
      filter(indicator == "share",
             trendscoarse == "15+") %>% 
      ggplot(aes(period, value, group = indicator, color = indicator)) +
      geom_line(size = 2) +
      geom_point(size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#2980b9") +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "", subtitle = "% Share of Index Positive Testing (Adults)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_index_pos_share.png", dpi = 300, width = 5.83, height = 2.44, units = "in")

   
  #viz positive index tests, ALL
    df_index_yield %>% 
      filter(indicator == "val") %>% 
      ggplot(aes(period, value, fill = indicator)) +
      geom_col(fill = "#2980b9") +
      scale_y_continuous(labels = comma) +
      labs(x = "", y = "", subtitle = "Positive Index Tests (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(), 
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_index_pos_all.png", dpi = 300, width = 5.83, height = 4.88, units = "in")
    
  
  #share for all
    df_index_share_all <- df %>% 
      filter(indicator == "HTS_TST",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
             trendscoarse != "Unknown Age",
             str_detect(period, "q")) %>%
      mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "Index", "Other")) %>% 
      group_by(period, statushiv, modality) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      group_by(period, modality) %>% 
      mutate(yield = val/sum(val)) %>%
      ungroup() %>% 
      mutate(period = str_remove(period, "20") %>% toupper) %>% 
      filter(statushiv == "Positive") %>% 
      group_by(period) %>% 
      mutate(share = val / sum(val)) %>% 
      ungroup() %>% 
      filter(modality == "Index") %>% 
      gather(indicator, value, val, yield, share) %>% 
      arrange(indicator, period)
  
  #viz share of positive index tests, ALL
    df_index_share_all %>% 
      filter(indicator == "share") %>% 
      ggplot(aes(period, value, group = indicator, color = indicator)) +
      geom_line(size = 2) +
      geom_point(size = 6) +
      expand_limits(y = 0) +
      scale_color_manual(values = "#2980b9") +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "", y = "", subtitle = "% Share of Index Positive Testing (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_index_pos_share_all.png", dpi = 300, width = 5.83, height = 4.88, units = "in")
    
  #data for agency
    df_index_agency <- df %>% 
      filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
             modality %in% c("Index", "IndexMod"),
             str_detect(period, "q")) %>% 
      group_by(period, fundingagency) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(period = str_remove(period, "20") %>% toupper,
             fundingagency = str_remove(fundingagency, "HHS/"))

  #viz agency breakdown
    df_index_agency %>% 
      ggplot(aes(period, val, fill = fundingagency)) +
      geom_col() +
      scale_fill_manual(values = c("#2980b9", "#27ae60", "#9b59b6")) +
      scale_y_continuous(label = comma) +
      facet_grid(. ~ fundingagency) +
      labs(x = "", y = "", subtitle = "Positive Index Tests (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_index_pos_agency.png", dpi = 300, width = 12.2, height = 6.7, units = "in")
    
  #data for agency share
    df_index_agency_share <- df %>% 
      filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
             str_detect(period, "q")) %>% 
      mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "Index", "Other")) %>% 
      group_by(period, fundingagency, modality) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      group_by(period, fundingagency) %>% 
      mutate(share = val/sum(val)) %>% 
      ungroup() %>% 
      filter(modality == "Index") %>% 
      mutate(period = str_remove(period, "20") %>% toupper,
             fundingagency = str_remove(fundingagency, "HHS/"),
             lab = case_when(period == "FY19Q2" ~ fundingagency))
    
  #viz agency share
    df_index_agency_share %>% 
      ggplot(aes(period, share, group = fundingagency, color = fundingagency)) +
      geom_line(size = 2) +
      geom_point(size = 6) +
      geom_text(aes(label = lab), hjust = -.5, na.rm = TRUE, fontface = "bold") +
      expand_limits(y = 0) +
      scale_color_manual(values = c("#2980b9", "#27ae60", "#9b59b6")) +
      scale_y_continuous(label = percent_format(1)) +
      labs(x = "", y = "", subtitle = "Share of Positive Index Tests (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
  
    ggsave("../Downloads/TZA_index_pos_agency_share.png", dpi = 300, width = 12.2, height = 6.7, units = "in")

    

# TX ----------------------------------------------------------------------

  #new
    df_tx <- df %>% 
      filter(indicator == "TX_NEW",
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "q")) %>%
      group_by(period,fundingagency) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fy = str_sub(period, 3,6),
             period = str_remove(period, "20") %>% toupper)

    df_tx_targ <- df %>% 
      filter(indicator == "TX_NEW",
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "targets")) %>%
      group_by(period, fundingagency) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fy = str_sub(period, 3,6),
             qtr_target = val/4) %>% 
      select(-period, -val)
    
    df_tx <- df_tx %>% 
      left_join(df_tx_targ, by = c("fy", "fundingagency"))
    
    df_tx_overall <- df_tx %>% 
      group_by(period) %>% 
      summarise_at(vars(val, qtr_target), sum, na.rm = TRUE) %>% 
      ungroup()
  
    #tx trend
    df_tx_overall %>% 
      ggplot(aes(period, val)) +
      geom_col(fill = "#2980b9") +
      geom_errorbar(aes(ymax=qtr_target, ymin=qtr_target), width=.8, size = 1, colour="#c0392b", na.rm = TRUE) + 
      scale_y_continuous(label = comma) +
      labs(x = "", y = "", subtitle = "TX_NEW (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_tx_new.png", dpi = 300, width = 12.2, height = 6.7, units = "in")

    #tx quarterly achievement by agency
    df_tx %>% 
      mutate(ach = val / (qtr_target * 4),
             lab = case_when(period == "FY19Q2" ~ fundingagency),
             fundingagency = str_remove(fundingagency, "HHS/")) %>% 
      ggplot(aes(period, ach, group = fundingagency, color = fundingagency)) +
      geom_hline(yintercept = .25, size = 1.5, linetype = "dashed", color = "#595959") +
      geom_line(size = 2) +
      geom_point(size = 6) +
      geom_text(aes(label = lab), hjust = -.5, na.rm = TRUE, fontface = "bold") +
      #expand_limits(y = 0) +
      scale_color_manual(values = c("#2980b9", "#27ae60", "#9b59b6")) +
      scale_y_continuous(label = percent_format(1)) +
      labs(x = "", y = "", subtitle = "Quarterly Contribution towards TX_NEW Target (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_tx_new_target_contr.png", dpi = 300, width = 12.2, height = 6.7, units = "in")
    
    
  #tx_curr
    df_tx_curr<- df %>% 
      filter(indicator %in% c("TX_CURR"),
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "q")) %>%
      group_by(period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(period = str_remove(period, "20") %>% toupper)
    
  #tx_curr trend
    df_tx_curr %>% 
      ggplot(aes(period, val)) +
      geom_col(fill = "#2980b9") +
      scale_y_continuous(label = comma) +
      labs(x = "", y = "", subtitle = "TX_CURR (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_tx_curr.png", dpi = 300, width = 12.2, height = 6.7, units = "in")
    
  #tx_net_new trend
    df_tx_curr %>% 
      mutate(net_new = val - lag(val)) %>% 
      filter(period != "FY18Q1") %>% 
      ggplot(aes(period, net_new)) +
      geom_col(fill = "#2980b9", na.rm = TRUE) +
      scale_y_continuous(label = comma) +
      labs(x = "", y = "", subtitle = "TX_NET_NEW (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_net_new.png", dpi = 300, width = 12.2, height = 6.7, units = "in")

# OVER TESTING ------------------------------------------------------------

    #testing data
    
    df_hts_trend <- df %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "q")) %>% 
      group_by(indicator, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fy = str_sub(period, 3, 6),
             period = str_remove(period, "20") %>% toupper)
    
    
    df_hts_trend %>% 
      ggplot(aes(period, val, color = indicator, group = indicator)) +
      geom_line(size = 2) +
      geom_point(size = 6) +
      scale_color_manual(values = c("#2980b9", "#27ae60")) +
      scale_y_continuous(label = comma) +
      expand_limits(y = 0) +
      labs(x = "", y = "", subtitle = "HTS_POS remains constant while HTS falls (Adults + Peds)") +
      facet_grid(indicator ~ ., scales = "free_y") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_over_testing.png", dpi = 300, width = 12.2, height = 6.7, units = "in")
    
    
    df_hts_trend_agency <- df %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "q")) %>% 
      group_by(indicator, fundingagency, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fy = str_sub(period, 3, 6),
             period = str_remove(period, "20") %>% toupper)
    
    df_hts_targs_agency <- df %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             str_detect(period, "targets")) %>% 
      group_by(indicator, fundingagency, period) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fy = str_sub(period, 3, 6),
             qtr_target = val /4) %>% 
      select(-period, -val)
    
    df_hts_trend_agency <- left_join(df_hts_trend_agency, df_hts_targs_agency, by = c("fy", "indicator", "fundingagency"))

    df_hts_trend_agency %>% 
      filter(indicator == "HTS_TST") %>% 
      mutate(ach = val / (qtr_target * 4),
             fundingagency = str_remove(fundingagency, "HHS/"),
             lab = case_when(period == "FY18Q1" ~ fundingagency)) %>% 
      ggplot(aes(period, ach, group = fundingagency, color = fundingagency)) +
      geom_hline(yintercept = .25, size = 1.5, linetype = "dashed", color = "#595959") +
      geom_line(size = 2) +
      geom_point(size = 6) +
      geom_text(aes(label = lab), hjust = 1.5, na.rm = TRUE, fontface = "bold") +
      expand_limits(y = 0) +
      scale_color_manual(values = c("#2980b9", "#27ae60", "#9b59b6")) +
      scale_y_continuous(label = percent_format(1)) +
      labs(x = "", y = "", subtitle = "Quarterly Contribution towards HTS_TST Target (Adults + Peds)") +
      theme_light() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            text = element_text(size = 12))
    
    ggsave("../Downloads/TZA_hts_target_contr.png", dpi = 300, width = 12.2, height = 6.7, units = "in")
    
    