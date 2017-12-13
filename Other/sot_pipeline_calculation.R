## SOT Pipeline calculation

pacman::p_load(tidyverse, janitor, readxl)

pbac <- "C:/Users/achafetz/Downloads/Malawi COP17 PBAC version trying to get pipeline by budget code.xlsx"

im_totals <- read_excel(pbac, sheet = "IM Subtotals", skip = 1)

im_totals <- im_totals %>% 
  clean_names(.) %>% 
  select(agency:mechanism_name, circ:hvab) %>% 
  slice(9:n()) %>% 
  filter(agency != "0" ) %>% 
  gather(budget_code, total_resources_needed, -agency:-mechanism_name) %>% 
  filter(total_resources_needed!=0) %>% 
  mutate(total_resources_needed = round(total_resources_needed, 2))

mo <- read_excel(pbac, sheet = "COBD")

mo <- mo %>% 
  clean_names(.) %>%
  slice(2:n()) %>% 
  gather(budget_code, total_resources_needed, -agency:-mechanism_name) %>% 
  filter(total_resources_needed!=0) %>% 
  mutate(total_resources_needed = round(total_resources_needed, 2))

im_totals <- bind_rows(im_totals, mo)
  rm(mo)

im_central <- read_excel(pbac, sheet = "IM New Funding", skip = 2)
  
im_central <- im_central %>% 
  clean_names(.) %>% 
  select(agency:mechanism_name, central_vmmc) %>% 
  slice(9:n()) %>% 
  filter(central_vmmc !=0) %>% 
  mutate(central_vmmc = round(central_vmmc, 2), 
         budget_code = "circ") 

im_budget <- full_join(im_totals, im_central)
  rm(im_totals, im_central)

im_new <- read_excel(pbac, sheet = "IM New Funding", skip = 2)

im_new <- im_new %>% 
  clean_names(.) %>% 
  select(agency:mechanism_name, circ_1:hvab_1) %>% 
  slice(9:n()) %>% 
  filter(agency != "0" ) %>% 
  rename_if(is.double, funs(str_sub(., end = -3L))) %>% 
  gather(budget_code, newfunding, -agency:-mechanism_name) %>% 
  filter(newfunding!=0) %>% 
  mutate(newfunding = round(newfunding, 2))

im_budget <- full_join(im_budget, im_new)
  rm(im_new)
  
im_budget <- im_budget %>% 
  mutate_at(vars(total_resources_needed, central_vmmc, newfunding), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(pipeline = round(total_resources_needed - central_vmmc - newfunding, 2),
         budget_code = str_to_upper(budget_code)) %>% 
  arrange(mechid, budget_code)

codes <- tribble(
   ~budget_code,         ~fcn,
         "HBHC",         "C&T",
         "HTXD",         "C&T",
         "HTXS",         "C&T",
         "HVCT",         "C&T",
         "HVTB",         "C&T",
         "PDCS",         "C&T",
         "PDTX",         "C&T",
         "HLAB",         "HSS",
         "HMBL",         "HSS",
         "HMIN",         "HSS",
         "HVMS",         "HSS",
         "HVSI",         "HSS",
         "OHSS",         "HSS",
         "HKID",         "OVC",
         "CIRC",  "Prevention",
         "HVAB",  "Prevention",
         "HVOP",  "Prevention",
         "IDUP",  "Prevention",
         "MTCT",  "Prevention"
  )

im_budget <- left_join(im_budget, codes) %>% 
  select(agency:mechanism_name, fcn, budget_code:pipeline)

write_csv(im_budget, "C:/Users/achafetz/Downloads/imxfcn_pipeline.csv")

rm(codes)

im_fcns <- im_budget %>%
  select(-budget_code) %>% 
  group_by_if(is.character) %>% 
  summarize_if(is.numeric, funs(sum(., na.rm = TRUE))) %>% 
  ungroup


write_csv(im_totals, "C:/Users/achafetz/Downloads/totals.csv")
