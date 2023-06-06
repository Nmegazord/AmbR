#' Import Ambr audit data
#'
#' @description This function imports Ambr audit data from CSV files in a specified directory
#' @param path The directory path from which to import the CSV files
#' @param schema An optional parameter to specify a schema to be applied to the data
#' @return A list of dataframes, each representing a parameter in the Ambr audit data
#' @export
#' @examples
#' # Example usage
#' \dontrun{
#' import_ambr_audit_data(utils::choose.dir(), schema = FALSE)
#' }
#'

#' @importFrom rlang .data
#' @export
import_ambr_audit_data <- function(path, schema = FALSE) {
  # Generates a list of files within the AuditData folder that end in "cv.csv"
  file_list <- list.files(
    path = path,
    pattern = "*cv.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(file_list) == 0) {
    warning("No files found in the provided directory")
    return(NULL)
  }

  # Imports the files
  df <- dplyr::bind_rows(purrr::map2(
    purrr::map(
      file_list,
      data.table::fread,
      sep = ",",
      header = TRUE,
      drop = 1
    ),
    file_list,
    cbind
  ))


  # # Check if required columns exist in the imported data
  # required_columns <- c("V2", "Name", "value", "date_time")
  # if (!all(required_columns %in% colnames(df))) {
  #   missing_columns <- required_columns[!required_columns %in% colnames(df)]
  #   stop(paste0("Required column(s) not found in imported data: ", paste(missing_columns, collapse = ", ")))
  # }

  # Dropping the vessels that were not inoculated
  df <- df |>
    dplyr::filter(.data$V2 %in% dplyr::pull(dplyr::select(dplyr::filter(
      df, .data$Name == "INOCULATED"
    ), .data$V2)))

  # gets rid of all date_something rows (redundant)
  df <- df |>
    dplyr::filter(!grepl("DATE", .data$Name))

  # Extracting the culture station and vessel number from the filename
  df <- df |>
    dplyr::mutate(culture_station = as.factor(
      stringr::str_extract(.data$V2, pattern = "(?<=^AuditData/CS[1234]/)CS[1234](?=_[[:digit:]]{1,2}_cv.csv$)")
    ),
    vessel_number = as.factor(as.numeric(
      stringr::str_extract(.data$V2, pattern = "(?<=^AuditData/CS[1234]/CS[1234]_)[[:digit:]]{1,2}(?=_cv.csv$)")
    ))) |>
    dplyr::select(-.data$V2)

  # Clean rename and factor data
  df <- df |>
    janitor::clean_names() |>
    dplyr::rename(parameter = .data$name) |>
    dplyr::mutate(
      parameter = tolower(.data$parameter),
      parameter = as.factor(.data$parameter),
      date_time = lubridate::dmy_hms(.data$date_time)
    )

  # Imports map and stitches it to the dataframe
  df <- append_map(df, append_by = c("culture_station", "vessel_number"))

  # Calculates time_from_inoculated
  when_inoculated <- df |>
    dplyr::filter(.data$parameter == "inoculated") |>
    dplyr::select(.data$value, .data$vessel_id) |>
    dplyr::rename(time_of_inoculum = .data$value)

  df <- df |>
    dplyr::left_join(when_inoculated, by = c("vessel_id")) |>
    dplyr::mutate(
      time_from_inoculation = difftime(.data$date_time, lubridate::dmy_hms(.data$time_of_inoculum), units = "hours"),
      time_from_inoculation = as.numeric(.data$time_from_inoculation)
    ) |>
    dplyr::select(-.data$time_of_inoculum)

  # adds schema if available
  if (schema) {
    schema <- data.table::fread(schema) |>
      dplyr::mutate(
        vessel_number = as.numeric(.data$vessel_number),
        vessel_number = as.factor(.data$vessel_number),
        culture_station = as.factor(.data$culture_station)
      )

    df <- df |>
      dplyr::left_join(schema, by = c("culture_station", "vessel_number")) |>
      dplyr::mutate(
        clone = as.factor(.data$clone),
        generation = as.factor(.data$generation),
        clone_generation_id = as.factor(.data$clone_generation_id)
      )
  }

  # split the data frame by 'parameter' and attempt to guess each value format
  df_split <- split(df, df$parameter) |>
    purrr::map(\(x) x |> dplyr::mutate(value = readr::parse_guess(.data$value)))

  # the date data have a non-standard format and need special handling
  df_split$inoculated <- df_split$inoculated |>
    dplyr::mutate(value = lubridate::dmy_hms(.data$value))

  return(df_split)
}
