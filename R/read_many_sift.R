#' Read Many SIFT Files
#'
#' @description This function is a wrapper around "read_sift" to read in multiple SIFT .csv files. The user must specify a specific table within the SIFT data (defaulting to "concentrations") and the function will output a single tibble with that table from all provided .csv files, tagged with the start time from each.
#'
#' @param files A vector of file paths.
#' @param table A table from the SIFT data. One of "time", "meta", "prep_phase", "sample_phase", "phase_mean_values", "intensity_corrected", "time_vs_mass", "concentrations", "analytes" or "summary". Defaults to "concentrations".
#' @param ... Arguments to pass to "read_sift"
#'
#' @return A tibble.
#' @export
#'

read_many_sift = function(files, table = "concentrations", ...){

  read_specific = function(file, table = table){

    all_sift = siftr::read_sift(file = file, chatty = FALSE, ...)

    single_table = all_sift[[table]] %>%
      dplyr::mutate(start_time = all_sift$time) %>%
      dplyr::relocate(start_time)

    return(single_table)

  }

  mapped_sift = purrr::map_dfr(.x = files, .f = ~read_specific(file = .x, table = table))

  return(mapped_sift)

}
