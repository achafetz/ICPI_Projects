## PROJECT: 
## AUTHOR:  A.CHAFETZ
## PURPOSE: Compare trend in NET NEW in Malawi
## DATE:    2019-02-13


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
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


# MUNGE -------------------------------------------------------------------


df_netnew <- df_mer %>% 
  filter(operatingunit == "Malawi",
         indicator == "TX_NET_NEW",
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, contains("q")) %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC")),
         pd = str_remove(pd, "20") %>% toupper(.),
         neg = val < 0) %>% 
  arrange(fundingagency, pd) 


# VISUALIZE ---------------------------------------------------------------


df_netnew %>% 
  ggplot(aes(pd, val, fill = neg)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "#595959") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c("#27ae60", "#d35400")) +
  facet_grid(fundingagency ~ .) +
  labs(x = "", y = "",
       title = "Positive Returns",
       subtitle = "USAID has seen positive TX Net New every quarter for the past two years",
       caption = "Source: FY18Q4c MSD") +
  plot_theme()

ggsave("~/COP19/Malawi/MWI_net_new_Trend.png", dpi = 300, width = 8, height = 5, units = "in")



