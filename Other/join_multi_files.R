#dependencies
  library(fs)
  library(tidyverse)

#create a temporary folder to store dummy files
  tmp <- dir_create(file_temp())

#create datasets & store in tmp folder
    #dataset A
    tribble(
        ~source, ~type, ~value,
            "a",   "X",    20L,
            "a",   "X",    98L,
            "a",   "X",    75L,
            "a",   "Y",    23L,
            "a",   "Z",    11L,
            "a",   "Z",    26L,
            "a",   "Z",    63L,
            "a",   "Z",    89L,
            "a",   "Z",    61L,
            "a",   "Z",    11L
        ) %>% 
    write_csv(file.path(tmp, "data_a.csv"))

    #dataset B
    tribble(
        ~source, ~type, ~value,
            "b",   "X",    41L,
            "b",   "X",    32L,
            "b",   "Y",    69L,
            "b",   "Y",    85L,
            "b",   "Y",     2L,
            "b",   "Y",    75L,
            "b",   "Z",    35L,
            "b",   "Z",    23L,
            "b",   "Z",    51L,
            "b",   "Z",    70L
        ) %>% 
    write_csv(file.path(tmp, "data_b.csv"))

#store filepath for tmp files
  files <- dir_ls(tmp)

#create function for purrr, read & filter to variable of interest
  limit <- function(path, keep_type){
      read_csv(path) %>% 
      filter(type == !!keep_type)
  }

#read in both files & join (purr)
  (df_join <- files %>% 
    map(~ limit(., "Z")) %>% 
    reduce(rbind))

#remove envir and tmp folder
  dir_delete(tmp)
  rm(df_join, files, tmp, limit)
  