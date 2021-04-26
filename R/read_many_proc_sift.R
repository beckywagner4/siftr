
#' Read Many Processed SIFT Files
#'
#' @description This function is a wrapper around "read_proc_sift" to read in multiple pre-processed SIFT .csv files. The user must specify a specific table within the SIFT data (defaulting to "analyte_conc") and the function will output a single tibble with that table from all provided .csv files.
#'
#' @param files A vector of file paths.
#' @param table A table from the SIFT data. One of "analyte_conc", "conc_per_reagent" or "conc_per_product". Defaults to "analyte_conc".
#'
#' @return A tibble.
#' @export
#'

read_many_proc_sift = function(files, table = "analyte_conc"){

  mapped_sift = purrr::map_dfr(.x = files,
                               .f = ~siftr::read_proc_sift(file = .x) %>% .[[table]])

  return(mapped_sift)

}
