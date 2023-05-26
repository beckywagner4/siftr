#' Read SIFT Data
#'
#' @description Raw SIFT data is very messy and hard to use. This function reads
#'   SIFT data from a .csv and "tidies" each element to enhance usability. The
#'   output is a list of each individual table that the SIFT outputs, as well as
#'   the date and time the SIFT began working. Also has functionality to specify
#'   if the PREPARATION PHASE is present or not.
#'
#' @param file The file path for the SIFT .csv file.
#' @param drop_prep Is the PREPARATION sub-data missing? TRUE/FALSE.
#' @param chatty \code{TRUE}/\code{FALSE}. Should the function communicate what it is doing? Useful for debugging.
#' @param warn logical. if a section of the file cannot be read, should a warning or error be thrown? Default FALSE leads to an error being produced. For use with \code{read_many_sift()} to skip bad files
#'
#' @return A list.
#' @export
#'

read_sift <- function(file, drop_prep = F, chatty = T, warn = FALSE) {
  if (chatty) {
    message(paste("Currently reading", file))
  }

  # Markers for the meta-data rows in the SIFT data
  flags <- c(
    "Mass Vs Time",
    "Cycle vs Product",
    "Time vs Mass",
    "Detailed Compound Concentrations",
    "Analyte vs Time",
    "Summary,"
  )

  # Read the lines in the SIFT data and mark them with row numbers
  lines <- tibble::tibble(line = readLines(file)) %>%
    dplyr::mutate(start = dplyr::row_number())

  # Calculate start and end lines for each section
  start_ends <- lines %>%
    dplyr::filter(stringr::str_detect(line, paste(flags, collapse = "|"))) %>%
    rbind(tibble::tibble(line = "start", start = 0)) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(end = dplyr::lead(start)) %>%
    tidyr::replace_na(list(end = max(lines$start))) %>%
    dplyr::mutate(start = dplyr::if_else(line == "Summary,:", start + 1, start))

  # Subfunction: Read in each bit of the data individually by mapping over our starts and ends
  read_data <- function(start, end) {
    if (start == 0) {
      df <- suppressMessages(suppressWarnings(readr::read_csv(file,
        col_types = readr::cols(), na = c(":", " ", ""),
        skip = start, n_max = end - start, col_names = F
      )))
    } else {
      df <- suppressMessages(suppressWarnings(readr::read_csv(file,
        col_types = readr::cols(), na = c(":", " ", ""),
        skip = start, n_max = end - (start + 2)
      )))
    }

    return(df)
  }

  # Map using the above function - returns a list.
  raw <- purrr::map2(.x = start_ends$start, .y = start_ends$end, .f = ~ read_data(start = .x, end = .y))

  if (chatty) {
    message("Read raw data")
  }

  # Clean and tidy each of the data frames in turn
  meta <- tryCatch({
    raw[[1]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      tidyr::drop_na(X2) %>%
      dplyr::select(-X3) %>%
      tidyr::pivot_wider(names_from = "X1", values_from = "X2") %>%
      janitor::clean_names()},
    error = function(e){NULL})

  if(is.null(meta)){
    if(warn){
      warning(paste0("unable to read meta from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read meta from file: ", file))
    }

    return(NULL)
  }

  if (chatty) {
    message("Read meta")
  }

  n <- 0

  if (drop_prep) {
    n <- n - 1
  } else {

    prep_phase <- tryCatch({
      raw[[n + 2]] %>%
        janitor::remove_empty(which = c("rows", "cols")) %>%
        tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity", names_transform = list(num = as.integer)) %>%
        janitor::clean_names()},
      error = function(e){NULL})

    if(is.null(prep_phase)){
      if(warn){
        warning(paste0("unable to read prep_phase from file: ", file, " returning NULL"))
      }else{
        stop(paste0("unable to read prep_phase from file: ", file))
      }
      return(NULL)
    }

    if (chatty) {
      message("Read prep_phase")
    }
  }

  sample_phase <- tryCatch({
    raw[[n + 3]] %>%
      dplyr::mutate(across(where(is.character), ~ dplyr::if_else(.x == "", NA_character_, .x))) %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      tidyr::pivot_longer(-c(1:2), names_to = "num", values_to = "intensity", names_transform = list(num = as.integer)) %>%
      janitor::clean_names()
  },
  error = function(e){NULL})

  if(is.null(sample_phase)){
    if(warn){
      warning(paste0("unable to read sample_phase from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read sample_phase from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read sample_phase")
  }

  phase_mean_values <- tryCatch({
    raw[[n + 4]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      janitor::clean_names()
    },
    error = function(e){NULL})

  if(is.null(phase_mean_values)){
    if(warn){
      warning(paste0("unable to read phase_mean_values from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read phase_mean_values from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read phase_mean_values")
  }

  intensity_corrected <- tryCatch({
    raw[[n + 5]] %>%
      dplyr::select(1:2, contains("SAMPLE")) %>%
      janitor::row_to_names(1) %>%
      tidyr::pivot_longer(-c(1:2),
                          names_to = "num", values_to = "intensity",
                          names_transform = list(num = as.integer)
      ) %>%
      janitor::clean_names() %>%
      dplyr::mutate(phase = "SAMPLE")},
    error = function(e){NULL})

  if(is.null(intensity_corrected)){
    if(warn){
      warning(paste0("unable to read intensity_corrected from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read intensity_corrected from file: ", file))
    }
    return(NULL)
  }

  if (!drop_prep) {
    intensity_corrected_prep <- tryCatch({
      raw[[n + 5]] %>%
        dplyr::select(1:2, contains("PREP")) %>%
        janitor::row_to_names(1) %>%
        tidyr::pivot_longer(-c(1:2),
                            names_to = "num", values_to = "intensity",
                            names_transform = list(num = as.integer)
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate(phase = "PREPARATION")},
      error = function(e){NULL})

    if(is.null(intensity_corrected_prep)){
      if(warn){
        warning(paste0("unable to read intensity_corrected_prep from file: ", file, " returning NULL"))
      }else{
        stop(paste0("unable to read intensity_corrected_prep from file: ", file))
      }
      return(NULL)
    }

    intensity_corrected <- rbind(intensity_corrected, intensity_corrected_prep)
  }

  if (chatty) {
    message("Read intensity_corrected")
  }

  time_vs_mass <- tryCatch({
    raw[[n + 6]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      tidyr::pivot_longer(-(1:10), names_to = "ion") %>%
      janitor::clean_names()},
    error = function(e){NULL})

  if(is.null(time_vs_mass)){
    if(warn){
      warning(paste0("unable to read time_vs_mass from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read time_vs_mass from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read time_vs_mass")
  }

  concentrations <- tryCatch({
    raw[[n + 7]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      tidyr::pivot_longer(-(1:2), names_to = "ion", values_to = "concentration") %>%
      tidyr::separate(ion, into = c("product_ion", "compound", "reagent_ion", "unit"), sep = " \\(|;") %>%
      dplyr::mutate(dplyr::across(where(is.character), stringr::str_remove_all, "\\)")) %>%
      janitor::clean_names()},
    error = function(e){NULL})

  if(is.null(concentrations)){
    if(warn){
      warning(paste0("unable to read concentrations from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read concentrations from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read concentrations")
  }

  analytes <- tryCatch({
    raw[[n + 8]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      tidyr::pivot_longer(-(1:2), names_to = "compound", values_to = "analyte") %>%
      janitor::clean_names()},
    error = function(e){NULL})

  if(is.null(analytes)){
    if(warn){
      warning(paste0("unable to read analytes from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read analytes from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read analytes")
  }

  summary <- tryCatch({
    raw[[n + 9]] %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      janitor::clean_names()},
    error = function(e){NULL})

  if(is.null(summary)){
    if(warn){
      warning(paste0("unable to read summary from file: ", file, " returning NULL"))
    }else{
      stop(paste0("unable to read summary from file: ", file))
    }
    return(NULL)
  }

  if (chatty) {
    message("Read summary")
  }

  # Pull start time from the metadata
  start_time <- meta$job_start_date %>% lubridate::ymd_hms()

  if (chatty) {
    message("Extracted start_time\n")
  }

  if (drop_prep) {
    sift <- list(
      time = start_time,
      meta = meta,
      sample_phase = sample_phase,
      phase_mean_values = phase_mean_values,
      intensity_corrected = intensity_corrected,
      time_vs_mass = time_vs_mass,
      concentrations = concentrations,
      analytes = analytes,
      summary = summary
    )
  } else {
    sift <- list(
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
  }

  # Assemble and return list

  return(sift)
}
