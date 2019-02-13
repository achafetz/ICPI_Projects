library(tidyverse)
library(readxl)
library(scales)
library(extrafont)

# PLOT THEME --------------------------------------------------------------

plot_theme <- function() {
  theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, color = "#595959"),
        panel.grid = element_line(color = "#ebebeb"),
        plot.title = element_text(size = 15, face = "bold", color = "black"),
        plot.caption = element_text(size = 11,  color = "#595959")
  )
}



df_datapack <- read_xls("../Downloads/DataPack_Malawi_02062019.xls",
                          sheet = "SNU x IM",
                          skip = 4)

df_mer <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20181221_v2_1.rds")


df_hts <- df_mer %>% 
  filter(operatingunit == "Malawi",
         fundingagency == "USAID",
         indicator %in% c("HTS_TST", "HTS_TST_POS"))

df_hts %>% 
  filter(standardizeddisaggregate %in% c("CommunityDeliveryPoint", "FacilityDeliveryPoint")) %>% 
  group_by(fundingagency, indicator, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2017_targets))



msd_targets <- df_mer %>% 
  filter(operatingunit == "Malawi",
         fundingagency == "USAID",
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result", 
                                         "Modality/Age Aggregated/Sex/Result", 
                                         "CommunityDeliveryPoint", "FacilityDeliveryPoint")) %>% 
  group_by(fundingagency, indicator, standardizeddisaggregate, modality) %>% 
  summarise_at(vars(fy2017_targets, fy2018_targets, fy2019_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(type = case_when(standardizeddisaggregate == "CommunityDeliveryPoint" ~ "Comm",
                          standardizeddisaggregate == "FacilityDeliveryPoint"  ~ "Fac",
                          str_detect(modality, "Mod")                          ~ "Comm",
                          TRUE                                                 ~ "Fac")) %>% 
  group_by(fundingagency, indicator, type) %>% 
  summarise_at(vars(fy2017_targets, fy2018_targets, fy2019_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(fy2017_targets = ifelse(indicator == "HTS_TST_POS", NA, fy2017_targets))



dp_targets <- df_datapack %>% 
  filter(str_detect(indicatorCode, "HTS_TST"), !str_detect(indicatorCode, "KeyPop")) %>% 
  group_by(indicatorCode) %>% 
  summarise_at(vars(DataPackTarget), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(indicatorCode = str_replace(indicatorCode, "T_", "T.")) %>% 
  separate(indicatorCode, into = c("indicator", "modality", NA, NA, NA, "resultstatus"), sep = "\\.") %>% 
  spread(resultstatus, DataPackTarget) %>% 
  mutate(Total = Negative + Positive) %>% 
  gather(resultstatus, val, -indicator, -modality) %>% 
  filter(resultstatus != "Negative") %>% 
  mutate(indicator = ifelse(resultstatus == "Positive", "HTS_TST_POS", "HTS_TST"),
         type = ifelse(str_detect(modality, "Mod"), "Comm", "Fac")) %>% 
  group_by(indicator, type) %>% 
  summarise(fy2020_targets = sum(val, na.rm = TRUE))

targets <- left_join(msd_targets, dp_targets)  %>% 
  rename_all(~ str_remove_all(., "20|_targets")) %>% 
  rename(fy20 = fy) %>% 
  gather(pd, val, contains("fy")) %>% 
  spread(indicator, val) %>% 
  mutate(`Positivity (%)` = round(HTS_TST_POS/HTS_TST, 3) *100) %>% 
  gather(indicator, val, -fundingagency:-pd) %>% 
  arrange(indicator, type, pd) %>%
  mutate(pd = toupper(pd),
         lab = case_when(pd == "FY17" & indicator == "HTS_TST" ~ type))

targets <- targets %>% 
  filter(indicator!= "Positivity (%)") %>% 
  group_by(pd, indicator) %>% 
  mutate(share = val / sum(val)) %>% 
  ungroup() %>% 
  left_join(targets, .)


targets %>% 
  ggplot(aes(pd, val, group = type, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 6, na.rm = TRUE) +
  geom_text(aes(label = lab), family = "Gill Sans MT",
            hjust = 1.6, na.rm = TRUE) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#34495e","#95a5a6"))  +
  facet_grid(indicator ~ ., scales = "free_y") +
  labs(x = "", y = "targets",
       title = "Declining Share of USAID Community Testing, FY17-FY19",
       subtitle = "Community testing targets for FY19 were a third of FY17 targets depsite projected higher positivity",
       caption = "Source: FY18Q4c MSD + COP20 Data Pack (initial)") +
  plot_theme()

targets %>% 
  filter(pd != "FY20") %>% 
  ggplot(aes(pd, val, group = type, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 6, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "#595959") +
  geom_text(aes(label = lab), family = "Gill Sans MT",
            hjust = 1.6, na.rm = TRUE) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#34495e","#95a5a6"))  +
  facet_grid(indicator ~ ., scales = "free_y") +
  labs(x = "", y = "targets",
       title = "Declining Share of USAID Community Testing, FY17-FY19",
       subtitle = "Community testing targets for FY19 were a third of FY17 targets despite projected higher positivity",
       caption = "Source: FY18Q4c MSD") +
  plot_theme()
  
ggsave("~/COP19/Malawi/USAID_CommTesting_Trend.png", dpi = 300, width = 8, height = 8, units = "in")

targets %>% 
  filter(indicator == "HTS_TST",
         type == "Comm",
         pd != "FY20") %>% 
  ggplot(aes(pd, val, group = type, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 6, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "#595959") +
  geom_text(aes(label = lab), family = "Gill Sans MT",
            hjust = 1.6, na.rm = TRUE) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#34495e","#95a5a6"))  +
  labs(x = "", y = "targets",
       title = "Declining Share of USAID Community Testing, FY17-FY19",
       subtitle = "Community testing targets for FY19 were a third of FY17 targets despite projected higher positivity",
       caption = "Source: FY18Q4c MSD") +
  plot_theme()

ggsave("~/COP19/Malawi/USAID_CommTestingonly_Trend.png", dpi = 300, width = 8, height = 5, units = "in")



targets %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
  ggplot(aes(pd, share, group = type, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 6, na.rm = TRUE) +
  geom_text(aes(label = lab), family = "Gill Sans MT",
            hjust = 1.6, na.rm = TRUE) +
  scale_y_continuous(labels = percent_format(1)) +
  scale_color_manual(values = c("#34495e","#95a5a6"))  +
  facet_grid(indicator ~ ., scales = "free_y") +
  labs(x = "", y = "targets",
       caption = "Source: FY18Q4c MSD + COP20 Data Pack (initial)") +
  plot_theme()
  


