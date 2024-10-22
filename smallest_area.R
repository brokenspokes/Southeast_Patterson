smallest_area <- function(x, value, percent) {
  total_value <- sum(x[[value]], na.rm = TRUE)

  value_goal = total_value * percent / 100
  returned_geometries <- data.frame()
  running_total <- 0

  return(value_goal)

  dplyr::mutate(x,
                area = sf::st_area(geometry),
                density = value / units::drop_units(area)
                ) |>
  dplyr::arrange(density) -> df

  for(i in 1:nrow(df)) {
    row_value <- df[[value]][i]
    running_total <- running_total + row_value
    returned_geometries <- rbind(returned_geometries, df[i,])
    if (running_total > value_goal) {break}
  }

  return(returned_geometries)
}
