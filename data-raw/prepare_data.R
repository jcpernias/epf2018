library(tidyverse)

# the dictionary describes how the variables should be read
epf_dict_path <- file.path('data-raw', 'epf_2018_dict.csv')
epf_dict <- read_delim(epf_dict_path, delim = ';', col_types = 'ccciiii')

## usethis::use_data("DATASET")
