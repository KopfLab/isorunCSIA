#' UTILITY FUNCTIONS

get_setting <- function(data, variable, default = "") {
  setting <- filter(data, Variable == variable)
  if (nrow(setting) == 0) return(default)
  else return(setting$Value)
}
