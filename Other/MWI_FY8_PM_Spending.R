## PROJECT: PM Spending
## AUTHOR:  Aaron Chafetz, USAID
## PURPOSE: Compare PM spending as a share of overall spend
## DATE:    2019-01-29


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)

folderpath <- "~/COP19/Malawi/PM Breakdown"

# Import Data -------------------------------------------------------------

folder <- "~/ICPI/Data"

df_er <- list.files(folder, "^ER", full.names = TRUE) %>% 
  read_tsv(col_types = cols(.default = "c"))


# Clean data --------------------------------------------------------------

df_er <- df_er %>% 
  rename_all(~str_remove_all(., " |-") %>% tolower()) %>%  
  mutate(fy2018 = as.double(fy2018),
         fundingagency = str_remove_all(fundingagency, "HHS/")) %>% 
  rename(expenditures = fy2018,
         mechanismid = mechanism,
         primepartner = primepartnername, 
         implementingmechanismname = mechanismname) 

df_er_mwi <- df_er %>% 
  filter(operatingunit == "Malawi")

df_er_mwi <- df_er_mwi %>%
  rename_official()


# Theme -------------------------------------------------------------------

plot_theme <- function(){
  theme_light() +
  theme(text = element_text(family = "Calibri Light", size = 14),
        plot.title = element_text(family = "Calibri", face = "bold", size = 16),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank()
        )
}

# Analysis ----------------------------------------------------------------

#what is the breakdown of M&O by partner?

glimpse(df_er_mwi)

df_er_mwi %>% 
  count(program, subprogram, wt = expenditures) %>% 
  print(n = Inf)

df_pm <- df_er_mwi %>% 
  mutate(prog_type = ifelse(program == "PM", "PM","All other")) %>% 
  group_by(fundingagency, mechanismid, primepartner, implementingmechanismname, prog_type) %>% 
  summarise_at(vars(expenditures), sum, na.rm = TRUE) %>% 
  group_by(fundingagency, mechanismid, primepartner, implementingmechanismname) %>% 
  mutate(share = expenditures / sum(expenditures),
         prog_type = factor(prog_type, c("PM", "All other"))) %>% 
  ungroup()

write_csv(df_pm, file.path(folderpath, "FY18_ER_MWI_PM_Expenditures.csv", na = ""))

#total exp by agency
df_ag_tot <- df_pm %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(expenditures), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(lab =  paste0("$", round(expenditures/1000000,1), "m"),
         agency_col = ifelse(fundingagency == "USAID", "#7E6BC9", "#6BB1C9"))

df_ag_tot %>% 
  ggplot(aes(reorder(fundingagency, expenditures), expenditures)) +
  geom_col(fill = df_ag_tot$agency_col) +
  geom_text(aes(label = lab), 
            hjust = -.5,
            family = "Calibri Light") +
  coord_flip() +
  expand_limits(y = 80000000)  +
  scale_y_continuous(labels = comma) +
  labs(y = "FY18 Total Expenditures", x = "",
       title = "USAID HAD THE HIGHEST SPENDING IN FY18",
       caption = "Source: FY18 ER Structured Dataset") +
  plot_theme() 

ggsave("agency_tot.png", path = folderpath, width = 6, height = 4, units = "in", dpi = 300)

#PM breakdown by agency
df_ag_pm <- df_pm %>% 
  filter(prog_type == "PM") %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(expenditures), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(lab =  paste0("$", ifelse(expenditures > 1000000, paste0(round(expenditures/1000000,1), "m"), comma(expenditures))),
         agency_col = ifelse(fundingagency == "USAID", "#7E6BC9", "#6BB1C9")) %>% 
  arrange(expenditures) %>% 
  mutate(fundingagency = as_factor(fundingagency))

df_ag_pm %>% 
  ggplot(aes(fundingagency, expenditures)) +
  geom_col(fill = df_ag_pm$agency_col) +
  geom_text(aes(label = lab), 
            hjust = -.5,
            family = "Calibri Light") +
  coord_flip() +
  expand_limits(y = 14000000)  +
  scale_y_continuous(labels = comma) +
  labs(y = "FY18 PM Expenditures", x = "",
       title = "CDC HAD THE HIGHEST PM SPENDING",
       cation = "Source: FY18 ER Structured Dataset") +
  plot_theme() 

ggsave("agency_pm.png", path = folderpath, width = 6, height = 4, units = "in", dpi = 300)

#PM Share agency
df_ag_share <- df_pm %>% 
  group_by(mechanismid) %>% 
  mutate(total = sum(expenditures, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(prog_type == "PM") %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(expenditures, total), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(share = expenditures / total) %>% 
  arrange(share) %>% 
  mutate(fundingagency = as_factor(fundingagency),
         agency_col = ifelse(fundingagency == "USAID", "#7E6BC9", "#6BB1C9"))

df_ag_share %>% 
  ggplot(aes(fundingagency, share)) + 
  geom_col(fill =df_ag_share$agency_col) +
  geom_text(label = percent(df_ag_share$share, accuracy = 1),
            family = "Calibri Light",
            hjust = -.5) +
  coord_flip() +
  expand_limits(y = .7)  +
  scale_y_continuous(labels = percent) +
  labs(y = "FY18 PM Share of Overall Expended", x = "",
       title = "USAID HAD A SMALL SHARE OF ITS TOTAL IN PM",
       cation = "Source: FY18 ER Structured Dataset") +
  plot_theme()

ggsave("agency_pm_share.png", path = folderpath, width = 12, height = 8, units = "in", dpi = 300)

#Unnattributable
df_subrec <- df_er_mwi %>% 
  mutate(class_type = ifelse(objectclass == "Subrecipient", "Subrecipient", "All other")) %>% 
  group_by(fundingagency, class_type) %>% 
  summarise_at(vars(expenditures), sum, na.rm = TRUE) %>% 
  group_by(fundingagency) %>% 
  mutate(total = sum(expenditures)) %>% 
  ungroup() %>% 
  filter(class_type == "Subrecipient") %>% 
  mutate(share = expenditures / total) %>% 
  arrange(share) %>% 
  mutate(fundingagency = as_factor(fundingagency),
         agency_col = ifelse(fundingagency == "USAID", "#7E6BC9", "#6BB1C9"))

df_subrec %>% 
  ggplot(aes(fundingagency, share)) +
  geom_col(fill = df_subrec$agency_col) +
  geom_text(label = percent(df_subrec$share, accuracy = 1),
            hjust = -.4) +
  coord_flip() +
  expand_limits(y = .6)  +
  labs(y = "FY18 Subrecipient Share of Overall Expended", x = "",
       title = "A LARGE SHARE OF USAID'S SPENDING IS UNATTRIBUTABLE",
       caption = "Source: FY18 ER Structured Dataset") +
  plot_theme() 

ggsave("agency_subrecipient.png", path = folderpath, width = 6, height = 4, units = "in", dpi = 300)

#partner breakdown
 df_partner <- df_pm %>% 
   filter(prog_type == "PM",
          fundingagency %in% c("USAID", "CDC")) %>% 
   unite(name, c(mechanismid, primepartner), sep = " ") %>% 
   arrange(desc(fundingagency), share) %>% 
   mutate(name = as_factor(name),
          fundingagency = as_factor(fundingagency),
          agency_col = ifelse(fundingagency == "USAID", "#7E6BC9", "#6BB1C9"))
 
 df_partner %>% 
   ggplot(aes(name, share)) +
   geom_col(fill = df_partner$agency_col) +
   geom_text(label = percent(df_partner$share, accuracy = 1),
             family = "Calibri Light",
             hjust = -.4) +
   expand_limits(y = .67) +
   coord_flip() +
   facet_grid(fundingagency ~ ., scales = "free_y") +
   labs(y = "FY18 PM Share of Overall Expended", x = "",
        title = "USAID AND CDC PARTNER SHARES OF PM",
        subtitle = "Overall, USAID's PM share is 14% compared to 27% for CDC",
        caption = "Source: FY18 ER Structured Dataset") +
   plot_theme() 
 
 ggsave("partner_pm.png", path = folderpath, width = 6, height = 4, units = "in", dpi = 300)
 
 
 