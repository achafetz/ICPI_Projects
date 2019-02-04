#identify the 5 acceleration districts (EMRs)
acceleration <- c("Blantyre District",
                  "Zomba District",
                  "Mangochi District",
                  "Machinga District",
                  "Chikwawa District")

names2 <- tibble::tribble(
            ~mechanismid,         ~name,
                  "14113",       "EGPAF",
                  "18025",  "Lighthouse",
                  "18142",     "SIFPO 2",
                  "17585",    "LINKAGES",
                  "16704",       "One C",
                  "18244",     "JHPIEGO",
                  "18234",       "EQUIP",
                  "18544",       "EGPAF",
                  "70185",      "Baylor",
                  "17097",         "PCI",
                  "14441",  "Lighthouse",
                  "18479", "SIFPO 2 BLM",
                  "18654",   "AIDS-free")
            



#subset to focus
df_mwi <- df_mer %>% 
  filter(indicator == "HTS_TST_POS",
         #fundingagency == "USAID",
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         psnu %in% acceleration,
         !mechanismid %in% c("00000", "00001")) %>% 
  group_by(fundingagency, mechanismid, primepartner, modality, implementingmechanismname) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  left_join(names2, by = "mechanismid") %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/"),
         name = paste0(name, " [",fundingagency, " ", mechanismid,"]"))
  
df_mwi <- df_mwi %>% 
  mutate(type = case_when(str_detect(modality, "Index") ~ "Index",
                          str_detect(modality, "Mod")   ~ "Community",
                          TRUE                          ~ "Facility")) %>% 
  group_by(name, type) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  #filter(fy2018apr !=0) %>% 
  group_by(name) %>% 
  mutate(share = fy2018apr / sum(fy2018apr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!str_detect(name, "^NA ")) %>% 
  mutate(share = ifelse(is.nan(share), 0, share)) 

#order
df_mwi <- df_mwi %>%
  select(-fy2018apr) %>% 
  spread(type, share, fill = 0) %>% 
  arrange(Index, Facility, Community) %>% 
  mutate(name = as_factor(name)) %>% 
  gather(type, share, -name)

df_mwi <- df_mwi %>% 
  mutate(#name = factor(name, order),
         type = factor(type, c("Index", "Facility", "Community")),
         color = case_when(type == "Index" ~ "#2166AC",
                           TRUE            ~ "#67A9CF")) %>% 
  arrange(name, type)

df_mwi %>% 
  ggplot(aes(name, share, fill = color)) +
  geom_col(fill = "#67A9CF")+
  coord_flip() +
  scale_y_continuous(labels = percent) +
  facet_wrap(. ~ type) +
  labs(title = "Share of Testing Type in Malawi",
       subtitle = "Limited to the 5 Districts with EMRs",
       caption = "Source: MER Structured Dataset (FY18Q4i) + ER Structured Dataset (FY18i)
       Districts: Blantyre, Zomba, Mangochi, Machinga, & Chikwawa",
       x = "", y = "") +
  theme_bw() +
  theme(legend.position  = "none",
        panel.border     = element_blank(),
        axis.ticks       = element_blank(),
        plot.title       = element_text(face = "bold", size = 14),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold", size = 12), 
        plot.caption     = element_text(color = "#404040"),
        axis.line.x      = element_line("#bfbfbf")) #lgray

