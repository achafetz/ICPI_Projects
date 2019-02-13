## PROJECT: PROXY LINAKGE
## AUTHOR:  A.CHAFETZ
## PURPOSE: review proxy linkage in FY18
## DATE:    2019-02-13

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)


path <- "~/Nigeria TDY/MWCC_FY18Q4_ahc.xlsx"
excel_sheets(path)

df_mwcc <- read_xlsx(path, sheet = "MWCC-FY18Q4_alt")

df_mwcc <- df_mwcc %>%
  rename_all(~str_replace_all(., " ", "_") %>% tolower(.)) %>%
  mutate(mechanismid = as.character(mechanismid))

df_lng <- df_mwcc %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(starts_with("hts_tst_pos_fy18q"), starts_with("tx_new_fy18q")),sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, contains("fy")) %>% 
  mutate(pd = str_replace(pd, "_f", ",f") %>% toupper(.),
         fundingagency = str_remove(fundingagency, "HHS/"))%>% 
  separate(pd, c("ind", "pd"), sep = ",") %>% 
  spread(ind, val) %>% 
  mutate(lab = case_when(pd == "FY18Q1" ~ fundingagency))

df_lng %>% 
  group_by(fundingagency) %>% 
  summarize_at(vars(HTS_TST_POS, TX_NEW), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(proxylink = round(TX_NEW/HTS_TST_POS, 2)*100)


df_lng %>% 
  mutate(proxylink = TX_NEW/HTS_TST_POS) %>% 
  ggplot(aes(pd, proxylink, color = fundingagency, group = fundingagency)) +
  geom_line(size = 1) +
  geom_point(size = 6) +
  geom_text(aes(label = lab),
            hjust = 1.5,
            family = "Gill Sans MT",
            fontface = "bold",
            na.rm = TRUE) +
  scale_y_continuous(label = scales::percent_format(1)) +
  scale_color_manual(values = c("#34495e","#95a5a6" ,"#2980b9")) +
  expand_limits(y = 0) +
  labs(x = "", y = "proxy linkage (TX_NEW/HTS_POS)",
       title = "Declining proxy linkage",
       subtitle = "FY18 cumulative proxy linkage is similar between USAID (80%) and CDC (82%), 
       but CDC has seen a greater quarterly decline in FY18.",
       caption = "Source: Malawi Q4 POART Data Submission") +
  theme_light() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT", size = 12, color = "#595959"),
        plot.title = element_text(size = 14, face = "bold", color = "#2980b9"),
        plot.caption = element_text(size = 10, color = "#595959"),
        axis.title = element_text(size = 10))

ggsave("~/COP19/Malawi/FY18ProxyLinkage.png", dpi = 300, width = 6, height = 4.5, units = "in")

 




