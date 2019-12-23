library(tidyverse)


#' Convert character variables to numeric
#'
#' @noRd
convert_vars <- function(varname, fmt, width, decimals, data) {
  field <- data[[varname]]
  if (fmt == 'i') {
    field <- readr::parse_integer(field)
  } else if (fmt == 'd') {
    before <- stringr::str_sub(field, 1, width - decimals)
    after <- stringr::str_sub(field, width - decimals + 1, width)
    field <-
      readr::parse_double(stringr::str_c(before, after, sep = '.'))
  }
  rlang::set_names(list(field), varname)
}

#' Process database files
#'
#' @noRd
process_epf_file <- function(file, dict) {
  input_filename <- paste0(file, '_2018.gz')

  # get dictionary
  db_dict <- dplyr::filter(dict, file == !!file)

  db_pos <- readr::fwf_positions(db_dict[['start']],
                                 db_dict[['end']],
                                 db_dict[['varname']])

  # Read raw data
  raw_path <- file.path('data-raw', input_filename)

  default_cols <- readr::cols(.default = readr::col_character())
  db <-
    readr::read_fwf(raw_path, db_pos, col_types = default_cols)

  # Transform variables
  db <- db_dict %>%
    dplyr::select(varname, fmt, width, decimals) %>%
    purrr::pmap_dfc(convert_vars, data = db)

  return(db)
}


# the dictionary describes how the variables should be read
epf_dict_path <- file.path('data-raw', 'epf_2018_dict.csv')
epf_dict <- readr::read_delim(epf_dict_path, delim = ';',
                              col_types = 'ccciiii')

hogar <- process_epf_file('hogar', epf_dict)
gastos <- process_epf_file('gastos', epf_dict)
miembros <- process_epf_file('miembros', epf_dict)

usethis::use_data(hogar, compress = 'xz', overwrite = TRUE, version = 3)
usethis::use_data(gastos, compress = 'xz', overwrite = TRUE, version = 3)
usethis::use_data(miembros, compress = 'xz', overwrite = TRUE, version = 3)

