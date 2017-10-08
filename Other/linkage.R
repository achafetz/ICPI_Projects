
library("readr")
library("tidyverse")
library("scales")

fvdata_mwi <- read_delim("C:/Users/achafetz/Documents/ICPI/Data/ICPI_FactView_Site_IM_Malawi_20170815_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

names(fvdata_mwi) <- tolower(names(fvdata_mwi))



df_lnkg <- fvdata_mwi %>%
  filter((indicator %in% c("TX_NEW", "HTS_TST_POS")), disaggregate=="Total Numerator", typefacility=="Y")

df_lnkg[is.na(df_lnkg)] <- 0

df_lnkg <- df_lnkg %>%
  mutate(fy2017cum = fy2017q1 + fy2017q2 + fy2015q3) %>%
  group_by(facility, facilityuid, psnu, psnuuid, fy17snuprioritization, indicator) %>%
  summarize_at(vars(fy2017cum), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  spread(indicator, fy2017cum) %>%
  mutate(proxy_lnkg = round(TX_NEW/HTS_TST_POS,3)) %>%
  filter(is.finite(proxy_lnkg)) %>%
  arrange(proxy_lnkg)

#return the missing/0 values back to NA
  df_lnkg[df_lnkg==0] <- NA
  names(df_lnkg) <- tolower(names(df_lnkg))
  
  
  
  df_lnkg <- df_lnkg %>% 
    group_by(psnu) %>% 
    arrange(psnu, proxy_lnkg) %>%
    mutate(id = row_number())
  
#graph
  
  ggplot(df_lnkg, aes(reorder(id, proxy_lnkg),proxy_lnkg)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(x="facility", y="proxy linkage") + 
    facet_wrap(~psnu) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  summary(df_lnkg$proxy_lnkg)
  
  ggplot(df_lnkg, aes(proxy_lnkg)) +
    geom_histogram() +
    scale_x_continuous(labels = scales::percent) +
    labs(x="proxy linkage", y="frequency") + 
    facet_wrap(~psnu)
  