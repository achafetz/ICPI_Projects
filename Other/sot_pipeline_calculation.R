##   SOT Pipeline calculation
##   A.Chafetz, USAID
##   Purpose: generate calculate pipeline by budget code
##   Date: Dec 13, 2017

## DEPENDENCIES
# MWI COP17 PBAC

 
## SETUP ------------------------------------------------------------------------------------------------------

#load packages
  pacman::p_load(tidyverse, janitor, readxl)
#set wd
  setwd("C:/Users/achafetz/Downloads/")
#PBAC file location  
  pbac <- "Malawi COP17 PBAC version trying to get pipeline by budget code.xlsx"

## DATA MUNGING -----------------------------------------------------------------------------------------------

#TOTAL RESOURCES NEEDED
  
  #import sheet
    im_totals <- read_excel(pbac, sheet = "IM Subtotals", skip = 1)
  #munge
    im_totals <- im_totals %>% 
      clean_names(.) %>% #covert to lower & remove spaces
      select(agency:mechanism_name, circ:hvab) %>% #select only columns for total resources need
      slice(9:n()) %>% #remove agency subtotals
      filter(agency != "0" ) %>% #remove extra rows at bottom
      gather(budget_code, total_resources_needed, -agency:-mechanism_name) %>% #reshape long
      filter(total_resources_needed!=0) %>% #remove unnessary blank rows
      mutate(total_resources_needed = round(total_resources_needed, 2)) #round values
    
  #import M&O total resources necessary (found seperately in PBAC)
    mo <- read_excel(pbac, sheet = "COBD")
  #munge
    mo <- mo %>% 
      clean_names(.) %>%
      slice(2:n()) %>% #remove blank row
      gather(budget_code, total_resources_needed, -agency:-mechanism_name) %>% #reshape long
      filter(total_resources_needed!=0) %>% #remove unnessary blank rows
      mutate(total_resources_needed = round(total_resources_needed, 2)) #round values


#VMMC CENTRAL FUNDING

  #import
    im_central <- read_excel(pbac, sheet = "IM New Funding", skip = 2)
  #munge  
    im_central <- im_central %>% 
      clean_names(.) %>% 
      select(agency:mechanism_name, central_vmmc) %>% #select only columns for total resources need
      slice(9:n()) %>% #remove agency subtotals
      filter(central_vmmc !=0) %>%  #remove extra rows at bottom
      mutate(central_vmmc = round(central_vmmc, 2), 
             budget_code = "circ") #add budget code that applies to central funding and round values
  
    
#NEW FUNDING

  #import  
    im_new <- read_excel(pbac, sheet = "IM New Funding", skip = 2)
  #munge
    im_new <- im_new %>% 
      clean_names(.) %>% 
      select(agency:mechanism_name, circ_1:hvab_1) %>% #select only columns for total resources need
      slice(9:n()) %>%  #remove agency subtotals
      filter(agency != "0" ) %>% #remove extra rows at bottom
      rename_if(is.double, funs(str_sub(., end = -3L))) %>% #remove sub of _1 from all (underscore due to presceeding budget code allocations)
      gather(budget_code, newfunding, -agency:-mechanism_name) %>% #reshape long
      filter(newfunding!=0) %>% #remove extra rows at bottom
      mutate(newfunding = round(newfunding, 2)) #round values

    
#FUNCTIONAL AREAS FROM SOT
    
  #budget code to function mapping
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
    
## COMBINE DATA -------------------------------------------------------------------------------------------------------

  #append M&O resources onto rest of IMs
    im_totals <- bind_rows(im_totals, mo)
    rm(mo)
    
  #merge central funding and totals
    im_budget <- full_join(im_totals, im_central)
    
  #merge new funding in
    im_budget <- full_join(im_budget, im_new)
    
  #add in functions
    im_budget <- left_join(im_budget, codes) %>% 
      select(agency:mechanism_name, fcn, budget_code:pipeline) #reorder columns
    
  #remove unneeded dfs  
    rm(im_totals, im_central, im_new, codes)


## PIPELINE CALCULATION ----------------------------------------------------------------------------------------------
 
  #calculate pipeline (=total resources - central fudning - new funding)   
    im_budget <- im_budget %>% 
      #convert NA to 0 for subtraction purposes
      mutate_at(vars(total_resources_needed, central_vmmc, newfunding), funs(ifelse(is.na(.), 0, .))) %>% 
      #pipeline and convert budget code to upper case
      mutate(pipeline = round(total_resources_needed - central_vmmc - newfunding, 2),
             budget_code = str_to_upper(budget_code)) %>% 
      #sort
      arrange(mechid, budget_code)
    
  #pipeline by functions
    im_budget %>%
      select(-mechanism_name, -budget_code) %>% 
      group_by_if(is.character) %>% 
      summarize_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
      mutate_if(is.numeric, funs(as.integer(.))) %>%
      print(n=100)

    
## EXPORT -----------------------------------------------------------------------------------------------------------
    
  write_csv(im_budget, "imxfcn_pipeline.csv")

  rm(codes)

