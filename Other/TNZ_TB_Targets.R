##   TNZ TB COP18 Targets
##   A.Chafetz, USAID
##   Purpose: Pull TB targets for A.Ficht
##   Date: 2018-12-03

##   Data Source: MSD Q4i OUxIM

df_mer <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20181115_v1_1.rds")

dta <- df_mer %>% 
  filter(operatingunit == "Tanzania",
         indicator == "TB_PREV") %>% 
  group_by(standardizeddisaggregate, numeratordenom) %>% 
  summarise_at(vars(fy2019_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(fy2019_targets !=0) %>% 
  mutate(standardizeddisaggregate = case_when(standardizeddisaggregate == "Total Denominator" ~ standardizeddisaggregate,
                                              numeratordenom == "D"                           ~ paste(standardizeddisaggregate, "(Denom)"), 
                                              TRUE                                            ~ standardizeddisaggregate),
          standardizeddisaggregate = factor(standardizeddisaggregate, 
                                           levels = c("TherapyType/NewExistingArt/HIVStatus",
                                                      "Age Aggregated/Sex/HIVStatus", 
                                                      "Total Numerator",
                                                      "TherapyType/NewExistingArt/HIVStatus (Denom)",
                                                      "Age Aggregated/Sex/HIVStatus (Denom)",
                                                      "Total Denominator")),
         clr = case_when(standardizeddisaggregate == "Total Denominator" ~ "#ece2f0",
                         standardizeddisaggregate == "Total Numerator"   ~ "#1c9099",
                         TRUE                                            ~ "#a6bddb")) %>% 
  arrange(standardizeddisaggregate) 

dta %>% 
  ggplot(aes(standardizeddisaggregate, fy2019_targets)) +
  geom_col(fill = dta$clr) +
  geom_text(aes(y =0, label = scales::comma(fy2019_targets)), size = 5, hjust = -.1) +
  coord_flip() +
  labs(title = "Tanzania TB_PREV Targets", subtitle = "COP18/FY19", x = "", y = "") +
  #theme_light() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        title = element_text(size = 20),
        text = element_text(size = 18))
  
  