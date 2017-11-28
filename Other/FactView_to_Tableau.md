Getting Fact View quaters into Tableau
================

``` r
pacman::p_load(tidyverse, lubridate)
```

Working with quarters in Tableau can be challenging with the way the ICPI Fact View datasets are structured. We'll walk through restructuring the dataset to allow you to work with it more easily in Tableau

CREATE TEST DATA FRAME TO WORK WITH
-----------------------------------

The Fact View dataset starts out in a semi wide form initially. Let's import it and subset it slightly to make it more manageable to work with.

| psnu | indicator | fy2017q1 | fy2017q2 | fy2017q3 |
|------|-----------|----------|----------|----------|
| x    | y         | \#       | \#       | \#       |

``` r
  #import data (change this to wherever your OU x IM dataset is located)
    df_factview <- read_tsv("~/ICPI/Data/ICPI_FactView_OU_IM_20171115_v1_1.txt") 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   MechanismID = col_integer(),
    ##   coarseDisaggregate = col_logical(),
    ##   FY2016Q1 = col_integer(),
    ##   FY2016Q2 = col_integer(),
    ##   FY2016Q3 = col_integer(),
    ##   FY2016Q4 = col_integer(),
    ##   FY2016APR = col_integer(),
    ##   FY2017_TARGETS = col_integer(),
    ##   FY2017Q1 = col_integer(),
    ##   FY2017Q2 = col_integer(),
    ##   FY2017Q3 = col_integer(),
    ##   FY2017Q4 = col_integer(),
    ##   FY2017APR = col_integer()
    ## )

    ## See spec(...) for full column specifications.

``` r
df_factview <- df_factview %>% 
  #rename all columns to lower case [persnal preference]
    rename_all(tolower) %>% 
  
  #filter to smaller subset to work with
    filter(indicator == "TX_NEW", disaggregate == "Total Numerator", operatingunit == "Rwanda") %>% 
  
  #limit variable columns we're working with 
    select(operatingunit, mechanismid, indicator, contains("fy2")) 

  #view dataset
    head(df_factview)
```

    ## # A tibble: 6 x 19
    ##   operatingunit mechanismid indicator fy2015q2 fy2015q3 fy2015q4 fy2015apr
    ##           <chr>       <int>     <chr>    <chr>    <chr>    <chr>     <chr>
    ## 1        Rwanda       10825    TX_NEW     3291     <NA>     <NA>      3291
    ## 2        Rwanda       14287    TX_NEW     2622      741      311      3674
    ## 3        Rwanda       18418    TX_NEW     <NA>     <NA>     <NA>      <NA>
    ## 4        Rwanda       17619    TX_NEW     <NA>     <NA>     <NA>      <NA>
    ## 5        Rwanda       17623    TX_NEW     <NA>     <NA>     <NA>      <NA>
    ## 6        Rwanda       17625    TX_NEW     <NA>     <NA>     <NA>      <NA>
    ## # ... with 12 more variables: fy2016_targets <chr>, fy2016q1 <int>,
    ## #   fy2016q2 <int>, fy2016q3 <int>, fy2016q4 <int>, fy2016apr <int>,
    ## #   fy2017_targets <int>, fy2017q1 <int>, fy2017q2 <int>, fy2017q3 <int>,
    ## #   fy2017q4 <int>, fy2017apr <int>

CLEAN UP
--------

To Tableau needs data in a long format (below) rather than wide (above). So, this means we'll need to melt it, or reshape it to a long format. Beore reshaping, we need to clean it up a bit first.

``` r
df_clean <- df_factview %>%
  
  #remove APR and TARGETS from the dataset since they are not quarterly data
    select(-contains("apr"), -contains("targets")) %>%
  
  #remove the fy from the start of the value column names to make it easier to work with when we convert from string to a date
    rename_at(vars(contains("fy2")), funs(str_replace(.,"fy", "") ))
  
  #view dataset
    head(df_clean)
```

    ## # A tibble: 6 x 14
    ##   operatingunit mechanismid indicator `2015q2` `2015q3` `2015q4` `2016q1`
    ##           <chr>       <int>     <chr>    <chr>    <chr>    <chr>    <int>
    ## 1        Rwanda       10825    TX_NEW     3291     <NA>     <NA>       NA
    ## 2        Rwanda       14287    TX_NEW     2622      741      311       NA
    ## 3        Rwanda       18418    TX_NEW     <NA>     <NA>     <NA>       NA
    ## 4        Rwanda       17619    TX_NEW     <NA>     <NA>     <NA>       NA
    ## 5        Rwanda       17623    TX_NEW     <NA>     <NA>     <NA>       NA
    ## 6        Rwanda       17625    TX_NEW     <NA>     <NA>     <NA>       97
    ## # ... with 7 more variables: `2016q2` <int>, `2016q3` <int>,
    ## #   `2016q4` <int>, `2017q1` <int>, `2017q2` <int>, `2017q3` <int>,
    ## #   `2017q4` <int>

MELT
----

Now that the data is cleaned up a bit, let's reshape it.

| psnu | indicator | period   | value |
|------|-----------|----------|-------|
| x    | y         | fy2017q1 | \#    |
| x    | y         | fy2017q2 | \#    |
| x    | y         | fy2017q3 | \#    |
| x    | y         | fy2017q4 | \#    |

``` r
df_long <- df_clean %>% 
  
  #melt dataset
    gather(pd, val, starts_with("2"), na.rm = TRUE)

  #view dataset
    head(df_long)
```

    ## # A tibble: 6 x 5
    ##   operatingunit mechanismid indicator     pd   val
    ##           <chr>       <int>     <chr>  <chr> <chr>
    ## 1        Rwanda       10825    TX_NEW 2015q2  3291
    ## 2        Rwanda       14287    TX_NEW 2015q2  2622
    ## 3        Rwanda           1    TX_NEW 2015q2  -627
    ## 4        Rwanda       13704    TX_NEW 2015q2   627
    ## 5        Rwanda       10954    TX_NEW 2015q2    34
    ## 6        Rwanda        7335    TX_NEW 2015q2    11

CONVERT QUATER TO DATE
----------------------

The other issue is that PEPFAR deals in fiscal quarter not calendar quarters. So, FY2017Q1 is starts Oct 2016, not Jan 2017. It's easier to work with dates.

| psnu | indicator | period   | date       | value |
|------|-----------|----------|------------|-------|
| x    | y         | fy2017q1 | 2016-10-01 | \#    |
| x    | y         | fy2017q2 | 2017-01-01 | \#    |
| x    | y         | fy2017q3 | 2017-04-01 | \#    |

``` r
df_long_date <- df_long %>% 
  
  #add date
    mutate(date = yq(pd)  %m-% months(3)) %>% #to change from fiscal quarter to date, need to subtract 3 mo
  
  #reorder
    select(operatingunit, mechanismid, indicator, pd, date, val)

head(df_long_date)
```

    ## # A tibble: 6 x 6
    ##   operatingunit mechanismid indicator     pd       date   val
    ##           <chr>       <int>     <chr>  <chr>     <date> <chr>
    ## 1        Rwanda       10825    TX_NEW 2015q2 2015-01-01  3291
    ## 2        Rwanda       14287    TX_NEW 2015q2 2015-01-01  2622
    ## 3        Rwanda           1    TX_NEW 2015q2 2015-01-01  -627
    ## 4        Rwanda       13704    TX_NEW 2015q2 2015-01-01   627
    ## 5        Rwanda       10954    TX_NEW 2015q2 2015-01-01    34
    ## 6        Rwanda        7335    TX_NEW 2015q2 2015-01-01    11
