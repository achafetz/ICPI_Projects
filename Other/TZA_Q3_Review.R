
#dependencies
  library(tidyverse)
  library(ICPIutilities)

#import data
  df_tza <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_FY17-18_20180815_v1_1.Rds") %>%
    filter(operatingunit == "Tanzania")

#offical names, add net new and have FY18 cum
  df_tza <- df_tza %>%
    rename_official() %>%
    combine_netnew("~/ICPI/Data") %>%
    add_cumulative()

#identify Q3 indicators
  ind_q3 <- df_tza %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    group_by(indicator) %>%
    summarise_at(vars(fy2018q3), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    filter(fy2018q3!=0,
           !indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID_Two_Twelve_Months",
                             "PMTCT_STAT_KnownatEntry_POSITIVE", "PMTCT_STAT_NewlyIdentified_Negative",
                             "PMTCT_STAT_NewlyIdentified_POSITIVE", "PMTCT_HEI_POS", "HTS_TST_NEG")) %>%
    pull(indicator)

#OU level trends
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(indicator) %>%
    summarise_at(vars(contains("fy2018q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(fy2018q2_gr = round((fy2018q2 - fy2018q1)/fy2018q1, 2)*100,
           fy2018q3_gr = round((fy2018q3 - fy2018q2)/fy2018q2, 2)*100) %>%
    arrange(fy2018q3_gr)

#OU level achievement
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(indicator) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
    filter(is.finite(achievement)) %>%
    arrange(achievement)


#USAID level trends
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator) %>%
    summarise_at(vars(contains("fy2018q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(fy2018q2_gr = round((fy2018q2 - fy2018q1)/fy2018q1, 2)*100,
           fy2018q3_gr = round((fy2018q3 - fy2018q2)/fy2018q2, 2)*100) %>%
    arrange(fy2018q3_gr)

#USAID level achievement
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
    filter(is.finite(achievement)) %>%
    arrange(achievement)

#USAID vs CDC achievement
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency %in% c("USAID", "HHS/CDC")) %>%
    group_by(fundingagency, indicator) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
    filter(is.finite(achievement)) %>%
    select(-c(fy2018cum, fy2018_targets)) %>%
    spread(fundingagency, achievement) %>%
    mutate(comp = ifelse(USAID < `HHS/CDC`, "under", "")) %>%
    arrange(USAID)

#USAID vs CDC results
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency %in% c("USAID", "HHS/CDC")) %>%
    group_by(fundingagency, indicator) %>%
    summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    spread(fundingagency, fy2018cum)

#USAID partners
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(mechanismid, primepartner, implementingmechanismname,indicator) %>%
    summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    filter(fy2018cum!=0) %>%
    distinct(mechanismid, primepartner, implementingmechanismname)

#USAID partner Q3 results
  df_tza %>%
    filter(standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator, mechanismid) %>%
    summarise_at(vars(fy2018q3), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    filter(fy2018q3!=0) %>%
    spread(mechanismid, fy2018q3)


#USAID partner performance
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator, mechanismid) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ach = round(fy2018cum/fy2018_targets, 3)*100) %>%
    filter(!is.nan(ach)) %>%
    select(-c(fy2018cum, fy2018_targets)) %>%
    spread(mechanismid, ach) %>%
    print(n = Inf)


#USAID partner performance - cascade
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator, mechanismid) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ach = round(fy2018cum/fy2018_targets, 3)*100) %>%
    filter(!is.nan(ach)) %>%
    select(-c(fy2018cum, fy2018_targets)) %>%
    spread(indicator, ach) %>%
    print(n = Inf)

#USAID partner performance - cascade <15
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result",
                                           "Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
           agecoarse %in% c("<15","15+"),
           fundingagency == "USAID") %>%
    group_by(indicator, mechanismid, agecoarse) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ach = round(fy2018cum/fy2018_targets, 3)*100) %>%
    filter(!is.nan(ach)) %>%
    select(-c(fy2018cum, fy2018_targets)) %>%
    spread(mechanismid, ach) %>%
    arrange(agecoarse, indicator) %>%
    unite(ind, indicator, agecoarse) %>%
    select(ind, `16787`, `18060`,	`18237`, `17103`, `16784`,	`17420`) %>%
    print(n = Inf)

#USAID testing modalities
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
           fundingagency == "USAID") %>%
    group_by(indicator, modality) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
    arrange(indicator, achievement)

#USAID partner performance of testing modalities
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
           fundingagency == "USAID") %>%
    group_by(indicator, modality, mechanismid) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ach = round(fy2018cum/fy2018_targets, 3)*100) %>%
    filter(!is.nan(ach)) %>%
    select(-c(fy2018cum, fy2018_targets)) %>%
    spread(mechanismid, ach) %>%
    print(n = Inf)

#USAID partner Index testing trends
  df_tza %>%
    filter(indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
           modality == "Index",
           fundingagency == "USAID") %>%
    group_by(mechanismid, modality) %>%
    summarise_at(vars(contains("q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup()

#USAID partner Index testing trends
  df_tza %>%
    filter(indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
           modality %in% c("VMMC", "TBClinic"),
           fundingagency == "USAID") %>%
    group_by(mechanismid, modality) %>%
    summarise_at(vars(contains("q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    select(mechanismid, modality, fy2017q3:fy2018q3)

#USAID quarterly distribution of testing modalities
  df_tza %>%
    filter(indicator == "HTS_TST",
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
           fundingagency == "USAID") %>%
    mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "Index/Index Mod", modality)) %>%
    group_by(modality) %>%
    summarise_at(vars(starts_with("fy2018q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate_at(vars(starts_with("fy2018q")), ~ round(./sum(., na.rm = TRUE), 2)*100) %>%
    arrange(desc(fy2018q3))

#Agency self Testing results
  df_tza %>%
    filter(indicator == "HTS_SELF",
           standardizeddisaggregate =="Total Numerator") %>%
    group_by(fundingagency) %>%
    summarise_at(vars(starts_with("fy2018q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup()


#Agency PrEP trend
  df_tza %>%
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate =="Total Numerator") %>%
    group_by(fundingagency) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100) %>%
    arrange(achievement)

#Agency PrEP trend
  df_tza %>%
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate =="Total Numerator") %>%
    group_by(fundingagency) %>%
    summarise_at(vars(starts_with("fy2018q")), ~ sum(., na.rm = TRUE)) %>%
    ungroup()

#USAID VMMC age breakdown
  df_tza %>%
    filter(indicator == "VMMC_CIRC",
           standardizeddisaggregate =="Age/Sex") %>%
    group_by(agefine) %>%
    summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement = round(fy2018cum/fy2018_targets, 2)*100)

#export dataset
  df_tza %>%
    filter(indicator %in% ind_q3,
           standardizeddisaggregate %in% c("Total Numerator", "Modality/Age/Sex/Result",
                                           "Modality/Age Aggregated/Sex/Result", "Age/Sex/HIVStatus",
                                            "Age Aggregated/Sex/HIVStatus")) %>%
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
    select(operatingunit, primepartner, fundingagency, mechanismid, implementingmechanismname, indicator, agecoarse, sex, modality, fy2017_targets:fy2018q3) %>%
    group_by_if(is.character) %>%
    summarise_at(vars(starts_with("fy")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, fy2017_targets:fy2018q3, na.rm = TRUE)


  #16787 – HTS_TST_POS
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "16787") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val) %>%
    mutate(positivity = round(HTS_TST_POS/HTS_TST, 3)*100)

#18060 – HTS_TST_POS
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "18060") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val) %>%
    mutate(positivity = round(HTS_TST_POS/HTS_TST, 3)*100)

#18060 – TX
  df_tza %>%
    filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "18060") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val)

#18237 – HTS_TST_POS
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "18237") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val) %>%
    mutate(positivity = round(HTS_TST_POS/HTS_TST, 3)*100)

#18237 – TX
  df_tza %>%
    filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "18237") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val)

#17103 – TX
  df_tza %>%
    filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "17103") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val)

#16784 - PrEP
  df_tza %>%
    filter(indicator =="PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "16784") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val)

#17420 – HTS_TST_POS
  df_tza %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Total Numerator",
           mechanismid == "17420") %>%
    select(-fy2016q4) %>%
    group_by(mechanismid, indicator) %>%
    summarise_at(vars(contains("q"), ends_with("targets")), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    gather(pd, val, -mechanismid, -indicator) %>%
    spread(indicator, val) %>%
    mutate(positivity = round(HTS_TST_POS/HTS_TST, 3)*100)

#linkage
  df_tza %>%
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>%
    group_by(indicator, mechanismid) %>%
    summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    spread(indicator, fy2018cum) %>%
    mutate(proxylinkage = round(TX_NEW/HTS_TST_POS, 2) *100)
