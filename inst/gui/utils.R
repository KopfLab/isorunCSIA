#' function to generate the list structure for
#' the borders of the parameter tables
generate_borders <- function(type, type_col = c("bool" = 1, "numeric" = 2), width = 2, color = "green") {
  border <- list(width = width, color = color)
  borders <- list(top = border, left = border, bottom = border, right = border)
  lapply(seq(0, length(type)-1)[type %in% names(type_col)], function(row) {
    modifyList(list(row = row, col = type_col[type[row+1]] %>% as.integer()), borders)
  })
}
