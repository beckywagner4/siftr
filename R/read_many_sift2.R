#' Read Many SIFT Files
#'
#' @description This function is an alternative wrapper around "read_sift" to
#'   read in multiple SIFT .csv files. Unlike "read_many_sift", this function
#'   does not require specifying a table as it reads in all of them. All files
#'   are bound and labelled with the starting time of the run.
#'
#' @param files A vector of file paths.
#' @param ... Arguments to pass to "read_sift"
#'
#' @return A list of tibbles.
#' @export
#'

read_many_sift2 <- function(files, ...) {
  read_sift_tbl <- function(file) {
    sift_data <- siftr::read_sift(file, ...)

    lst_to_df <- function(n) {
      name <- names(sift_data)[n]

      if (n == 1) {
        tibble::tibble(start_time := sift_data[[n]])
      } else {
        tibble::tibble({{ name }} := list(sift_data[[n]]))
      }
    }

    purrr::map_dfc(1:length(sift_data), lst_to_df)
  }

  all_tbls <- purrr::map_dfr(files, read_sift_tbl)

  nested <- all_tbls |>
    tidyr::pivot_longer(-start_time) |>
    dplyr::rowwise() |>
    dplyr::mutate(value = list(dplyr::mutate(value, start_time = start_time))) |>
    dplyr::mutate(value = list(dplyr::relocate(value, start_time))) |>
    dplyr::group_by(name) |>
    dplyr::summarise(value = list(dplyr::bind_rows(value)))

  output_list <- nested |>
    dplyr::rowwise() |>
    dplyr::mutate(lst = list(name = (value))) |>
    dplyr::pull(lst)

  names(output_list) <- nested$name

  return(output_list)
}
