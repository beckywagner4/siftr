#' Read SIFT Data
#'
#' @description Raw SIFT data is very messy and hard to use. This function reads SIFT data from a .csv and "tidies" each element to enhance usability. The output is a list of each individual table that the SIFT outputs, as well as the date and time the SIFT began working.
#'
#' @param file The file path for the SIFT .csv file.
#'
#' @return A list.
#' @export
#'

read_sift = function(file){

  # Markers for the meta-data rows in the SIFT data
  flags = c("Mass Vs Time",
            "Cycle vs Product",
            "Time vs Mass",
            "Detailed Compound Concentrations",
            "Analyte vs Time",
            "Summary,")

  # Read the lines in the SIFT data and mark them with row numbers
  lines = tibble::tibble(line = readLines(file)) %>%
    dplyr::mutate(start = dplyr::row_number())

  # Calculate start and end lines for each section
  start_ends = lines %>%
    dplyr::filter(stringr::str_detect(line, paste(flags, collapse = "|"))) %>%
    rbind(tibble::tibble(line = "start", start = 0)) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(end =dplyr::lead(start)) %>%
    tidyr::replace_na(list(end = max(lines$start))) %>%
    dplyr::mutate(start = dplyr::if_else(line == "Summary,:", start + 1, start))

  # Subfunction: Read in each bit of the data individually by mapping over our starts and ends
  read_data = function(start, end){

    if(start == 0){

      df = suppressWarnings(readr::read_csv(file, col_types = readr::cols(), na = c(":"," ",""),
                                            skip = start, n_max = end - start, col_names = F))

    } else {

      df = suppressWarnings(readr::read_csv(file, col_types = readr::cols(), na = c(":"," ",""),
                                            skip = start, n_max = end - (start+2)))

    }

    return(df)

  }

  # Map using the above function - returns a list.
  raw = purrr::map2(.x = start_ends$start, .y = start_ends$end, .f = ~read_data(start = .x, end = .y))

  # Clean and tidy each of the data frames in turn
  meta = raw[[1]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::drop_na(X2) %>%
    dplyr::select(-X3) %>%
    tidyr::pivot_wider(names_from = "X1", values_from = "X2") %>%
    janitor::clean_names()

  prep_phase = raw[[2]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity", names_transform = list(num = as.integer)) %>%
    janitor::clean_names()

  sample_phase = raw[[3]] %>%
    dplyr::mutate(across(where(is.character), ~if_else(.x == "", NA_character_, .x))) %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity", names_transform = list(num = as.integer)) %>%
    janitor::clean_names()

  phase_mean_values = raw[[4]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    janitor::clean_names()

  intensity_corrected = rbind(
    raw[[5]] %>%
      dplyr::select(X1, X2, contains("PREP")) %>%
      janitor::row_to_names(1) %>%
      tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity",
                   names_transform = list(num = as.integer)) %>%
      janitor::clean_names() %>%
      dplyr::mutate(phase = "PREPARATION"),

    raw[[5]] %>%
      dplyr::select(X1, X2, contains("SAMPLE")) %>%
      janitor::row_to_names(1) %>%
      tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity",
                   names_transform = list(num = as.integer)) %>%
      janitor::clean_names() %>%
      dplyr::mutate(phase = "SAMPLE")
  )

  time_vs_mass = raw[[6]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::pivot_longer(-(1:10), names_to = "ion") %>%
    janitor::clean_names()

  concentrations = raw[[7]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::pivot_longer(-(1:2), names_to = "ion", values_to = "concentration") %>%
    tidyr::separate(ion, into = c("product_ion", "compound", "reagent_ion", "unit"), sep = " \\(|;") %>%
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_remove_all, "\\)")) %>%
    janitor::clean_names()

  analytes = raw[[8]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    tidyr::pivot_longer(-(1:2), names_to = "compound", values_to = "analyte") %>%
    janitor::clean_names()

  summary = raw[[9]] %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    janitor::clean_names()

  # Pull start time from the metadata
  start_time = meta$job_start_date %>% lubridate::ymd_hms()

  # Assemble and return list
  sift = list(
    time = start_time,
    meta = meta,
    prep_phase = prep_phase,
    sample_phase = sample_phase,
    phase_mean_values = phase_mean_values,
    intensity_corrected = intensity_corrected,
    time_vs_mass = time_vs_mass,
    concentrations = concentrations,
    analytes = analytes,
    summary = summary
  )

  return(sift)

}
