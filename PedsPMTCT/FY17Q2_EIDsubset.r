## DEPENDENT LIBRARIES ##
library("tidyverse")
library("readr")
#import data and call the dataframe factviewdata
setwd("C:/Users/achafetz/Documents/ICPI/Data/")
fvdata <- read_delim("ICPI_FactView_PSNU_IM_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
names(fvdata) <-tolower(names(fvdata))
df_u1 <- fvdata %>%
  filter((indicator %in% c("HTS_TST", "HTS_TST_POS") & age=="<01") | indicator %in% c("PMTCT_EID", "PMTCT_EID_POS_2MO", "PMTCT_EID_POS_12MO") )

write.table(df_u1, "C:/Users/achafetz/Downloads/psnu_im_hts_u1_pmtct_q2v1.txt", na="", sep = "\t")

unique(df_u1$indicator)

table(df_u1$indicator, df_u1$fy2017q2)
