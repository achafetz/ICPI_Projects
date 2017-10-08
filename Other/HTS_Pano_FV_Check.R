## Compare HTS Values - Fact View to Panorama

# Panorama setup - 
#   Disaggregates & Narratives > Age/Sex Disaggregate Tables
#   Filter to Age & change table to Most Complete Coarse
#   Remove Support Type and keep FY16 Cum. Results, FY17 Cum. Results, and FY17 Targets
#   Select only HTS_TST from indicator list


# Fact View Comparable Output

# DEPENDENT LIBRARIES ##
    library("tidyverse")
    library("readr")

#import data and call the dataframe factviewdata
    setwd("/Users/Aaron/Desktop/")
    fvdata <- read_delim("ICPI_FactView_OU_IM_20170515_v1_1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
        names(fvdata) <- tolower(names(fvdata))

#df with HTS values to compare to Panorama
    df_hts <- fvdata %>%
        filter(indicator == "HTS_TST", ismcad == "Y") %>%
        rowwise() %>%
        mutate (fy2017cum = sum(fy2017q1, fy2017q2, na.rm=TRUE)) %>%
        ungroup() %>%
        select(operatingunit, age, fy2016apr, fy2017cum, fy2017_targets) %>%
        group_by(operatingunit, age) %>%
        summarize_each(funs(sum(., na.rm=TRUE)))

#export
    write.csv(df_hts, "hts_global.csv", na="")
