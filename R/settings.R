# retrieve package settings, internal function, not exported
default <- function(name, allow_null = FALSE) {
  name <- enquo(name) %>% quos_to_text(variable = "setting")
  value <- getOption(paste0("isorunCSIA.", name))
  if (!allow_null && is.null(value)) stop("isorunCSAI setting '", name, "' does not exist, make sure to set the default first", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !paste0("isorunCSIA.", name) %in% names(options()))
    options(list(value) %>% setNames(paste0("isorunCSIA.", name)))
  return(invisible(value))
}

# turn debug on
turn_debug_on <- function() {
  set_default("debug", TRUE)
}

#' Set default database connection or pool
#' @param con database connection or pool object
#' @export
csia_set_db_con <- function(con) {
  set_default("con", enquo(con))
}

#' Set the instrument ID
#'
#' Note that this is not checked wheter it exists, simply used as the default for other functions.
#'
#' @param group_id group
#' @export
csia_set_instrument_id <- function(instrument_id) {
  set_default("instrument_id", instrument_id)
}
