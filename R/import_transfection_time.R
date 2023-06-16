#' Import and clean transfection time data
#'
#' @description This function imports and cleans transfection time data from a CSV file
#' @param path The directory path from which to import the CSV files
#' @return A dataframe with the cleaned transfection time data
#' @export
#' @examples
#' ## Simulate some example data
#' example_data <- data.frame(
#'   Name = c("TRANSFECTED", "VALUE_1", "VALUE_2"),
#'   Value = c("Yes", "1.2", "2.3"),
#'   Filename = c("file1.csv", "file1.csv", "file1.csv")
#' )
#' ## Write it to a temporary file
#' example_file <- tempfile(fileext = ".csv")
#' write.csv(example_data, example_file, row.names = FALSE)
#'
#' ## Now you can use the example file with your function
#' import_transfection_time(path = example_file)
#'

import_transfection_time <- function(path = utils::choose.dir()) {
  process_audit <-
    readr::read_csv(file = path,
                    col_names = FALSE,
                    skip = 1,
                    show_col_types = FALSE)

  # Data cleaning
  process_audit <- process_audit |>
    dplyr::mutate(X1 = stringr::str_replace_all(.data$X1, ';', ','),
                  X1 = stringr::str_sub(.data$X1, 2, -1)) |>
    tidyr::separate(
      col = .data$X1,
      into = c(
        "date_time",
        "action",
        "value",
        "source_location",
        "source_id",
        "source_well",
        "target_location",
        "target_id",
        "target_well",
        "extra_info"
      ),
      sep = ","
    ) |>
    dplyr::rename(vessel_number = .data$target_well,
                  culture_station = .data$target_id) |>
    dplyr::filter(.data$culture_station %in% c("CS1", "CS2", "CS3", "CS4")) |>
    dplyr::mutate(
      vessel_number = as.numeric(.data$vessel_number),
      culture_station = as.factor(.data$culture_station),
      vessel_number = as.factor(.data$vessel_number),
      date_time = lubridate::dmy_hms(.data$date_time)
    )

  # maps the position to vessel IDs (1 to 48) and appends them to the dataframe
  process_audit <- process_audit |>
    dplyr::left_join(
      tidyr::crossing(
        culture_station = process_audit$culture_station,
        vessel_number = process_audit$vessel_number
      ) |>
        dplyr::rename(culture_station = 1, vessel_number = 2) |>
        tibble::rowid_to_column("vessel_id") |>
        dplyr::mutate(
          vessel_number = as.factor(.data$vessel_number),
          vessel_number = as.factor(.data$vessel_number)
        ),
      by = c("culture_station", "vessel_number")
    ) |>
    dplyr::mutate(vessel_id = as.factor(.data$vessel_id))

  # Extract transfection time from process_audit and save it in a dataframe
  transfection_time <- process_audit |>
    dplyr::filter(
      .data$source_id %in% c(
        "Transfection Plate 1 CS1",
        "Transfection Plate 2 CS2",
        "Transfection Plate 3 CS3",
        "Transfection Plate 4 CS4"
      )
    ) |>
    dplyr::select(.data$date_time, .data$culture_station, .data$vessel_number, .data$vessel_id)
  return(transfection_time)
}
