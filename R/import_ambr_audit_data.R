#' Import Ambr audit data
#'
#' @description This function imports Ambr audit data from CSV files in a specified directory
#' @param path The directory path from which to import the CSV files
#' @param schema An optional parameter to specify a schema to be applied to the data
#' @param discard_non_inoculated A parameter that defaults as TRUE to discard the vessel data if vessel was not inoculated
#' @return A list of dataframes, each representing a parameter in the Ambr audit data
#' @export
#' @examples
#' # Example usage
#' #' import_ambr_audit_data("files", schema = FALSE)
#'

#' @importFrom rlang .data
#' @export
import_ambr_audit_data <- function(path, schema = FALSE, discard_non_inoculated = TRUE) {

  # Prints a message indicating that the file list is being generated
  message("Generating a list of files...")

  # Creates a list of the full file paths for all CSV files in the specified directory and its subdirectories
  file_list <- list.files(
    path = normalizePath(path),
    pattern = "*cv.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  # If no files were found, warn the user and return NULL
  if (length(file_list) == 0) {
    warning("No files found in the provided directory")
    return(NULL)
  }

  # Prints a message indicating that the files are being imported
  message("Importing files...")

  # Reads each CSV file into a dataframe, converting the 'Value' column to character,
  # and adds the filename as a new column, then combines all dataframes into one
  df <- dplyr::bind_rows(purrr::map(
    file_list,
    function(file) {
      df <- data.table::fread(input = file, sep = ",", header = TRUE, drop = 1)
      df$Value <- as.character(df$Value)
      df$Filename <- basename(file)
      df
    }
  ))

  # If discard_non_inoculated is TRUE, filters the data to only include files that contain "INOCULATED"
  if (discard_non_inoculated == TRUE) {
    message("Filtering data...")
    df <- df |>
      dplyr::filter(.data$Filename %in% dplyr::pull(dplyr::select(dplyr::filter(
        df, .data$Name == "INOCULATED"
      ), .data$Filename)))
  }

  # Filters out rows where 'Name' column contains "DATE"
  df <- df |>
    dplyr::filter(!grepl("DATE", .data$Name))

  # Prints a message indicating that the culture station and vessel number are being extracted
  message("Extracting culture station and vessel number...")

  # Extracts the culture station and vessel number from the filename and adds them as new columns,
  # then removes the 'Filename' column
  df <- df |>
    dplyr::mutate(
      culture_station = as.factor(stringr::str_extract(Filename, "(CS\\d+)")),
      vessel_number = as.factor(stringr::str_extract(Filename, "_([\\d]+)") |> stringr::str_replace_all("_", ""))
    ) |>
    dplyr::select(-Filename)

  # Prints a message indicating that the data is being cleaned
  message("Cleaning data...")

  # Cleans the names of the columns, renames 'name' column to 'parameter',
  # converts 'parameter' to lower case and factor, and converts 'date_time' to a datetime object
  df <- df |>
    janitor::clean_names() |>
    dplyr::rename(parameter = .data$name) |>
    dplyr::mutate(
      parameter = tolower(.data$parameter),
      parameter = as.factor(.data$parameter),
      date_time = lubridate::dmy_hms(.data$date_time)
    )

  # Prints a message indicating that the map is being appended
  message("Appending map...")

  # Appends a map to the dataframe (this function should be defined elsewhere in your code)
  df <- append_map(df, append_by = c("culture_station", "vessel_number"))

  # Prints a message indicating that the time from inoculation is being calculated
  message("Calculating time from inoculated...")

  # Creates a lookup table of inoculation times and uses it to calculate the time from inoculation for each row,
  # then removes the 'time_of_inoculum' column
  when_inoculated <- df |>
    dplyr::filter(.data$parameter == "inoculated")  |>
    dplyr::select(.data$value, .data$vessel_id) |>
    dplyr::rename(time_of_inoculum = .data$value)

  df <- df |>
    dplyr::left_join(when_inoculated, by = c("vessel_id")) |>
    dplyr::mutate(
      time_of_inoculum = lubridate::dmy_hms(.data$time_of_inoculum),
      time_from_inoculation = difftime(.data$date_time, .data$time_of_inoculum, units = "hours"),
      time_from_inoculation = as.numeric(.data$time_from_inoculation)
    ) |>
    dplyr::select(-.data$time_of_inoculum)

  # If a schema was provided, prints a message indicating that the schema is being added,
  # then adds the schema to the dataframe
  if (schema) {
    message("Adding schema...")
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

  # Prints a message indicating that the dataframe is being split
  message("Splitting the data frame...")

  # Splits the dataframe by 'parameter' column and guesses the class of 'value' column in each dataframe,
  # then converts 'value' column in 'inoculated' dataframe to a datetime object
  df_split <- split(df, df$parameter) |>
    purrr::map(\(x) x |> dplyr::mutate(value = readr::parse_guess(.data$value)))

  df_split$inoculated <- df_split$inoculated |>
    dplyr::mutate(value = lubridate::dmy_hms(.data$value))

  # Prints a message indicating that the data import is complete
  message("Data import complete.")

  # Returns the list of dataframes
  return(df_split)
}

