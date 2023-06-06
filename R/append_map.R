#' Appends missing mapping values to the dataframe
#'
#' @description This function joins the input dataframe with the ambr map on
#' the specified columns.
#' @param data The dataframe to which the ambr map will be appended.
#' @param append_by The columns on which to join the dataframe and the ambr map.
#' @return A dataframe with the ambr map appended.
#' @examples
#' # Example usage
#' df <- data.frame(culture_station = c("CS1", "CS2"), vessel_number = c(1, 2))
#' append_map(df, c("culture_station", "vessel_number"))
#'
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#' @export
append_map <- function(data, append_by) {
  # Generate the ambr map dataframe
  map <- data.frame(
    culture_station = rep(c("CS1", "CS2", "CS3", "CS4"), each = 12),
    vessel_number = rep(1:12, times = 4),
    vessel_id = 1:48
  )

  map <- map |>
    dplyr::mutate(
      culture_station = as.factor(.data$culture_station),
      vessel_number = as.factor(.data$vessel_number),
      vessel_id = as.factor(.data$vessel_id)
    )

  # Convert vessel_number of input dataframe to factor
  data$vessel_number <- as.factor(data$vessel_number)

  # Perform left join with the input dataframe
  x <- data |>
    dplyr::left_join(map, by = append_by)

  return(x)
}
