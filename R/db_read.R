
# logs ====

#' Retrieve instrumnet logs
#'
#' Returns instrument logs. Sorted by descending order (i.e. latest record first).
#'
#' @param mode which mode(s) to get logs for. If undefined, get logs for all modes.
#' @param filter what filter conditions to apply, if any (forwarded to \link[dplyr]{filter})
#' @param select what columns to select (forwarded to \link[select]{select})
#' @param max_rows if provided, only selects the indicated number of rows (more efficient this way than part of the filter)
#' @param convert_to_TZ if provided, converts the log_datetime to the provided timezone (by default the local one stored in \code{Sys.timezone()}). If NULL, will keep it as UTC.
#' @return instrument logs
#' @export
csia_get_instrument_logs <- function(
  instrument_id = default(instrument_id), mode = NULL, filter = NULL,
  select = everything(),
  max_rows = NULL,
  convert_to_TZ = Sys.timezone(),
  con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  instrument_id_value <- instrument_id

  if (!quiet) {
    glue::glue("Info: retrieving instrument logs for '{instrument_id_value}' ",
         if (!is.null(mode)) "in {paste(mode, collapse = ', ')} mode" else "in all modes",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "{if(!is.null(max_rows)) str_c(\", limited to \", max_rows, \" rows max\") else ''}... ") %>%
    message(appendLF = FALSE)
  }

  logs <- tbl(con, "instrument_logs") %>%
    arrange(desc(instrument_log_id)) %>%
    dplyr::filter(instrument_id == instrument_id_value) %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    {
      if (!is.null(max_rows)) dplyr::filter(., dplyr::row_number() <= max_rows)
      else .
    } %>%
    dplyr::select(!!select_quo) %>%
    collect()


  if (!quiet) glue::glue("found {nrow(logs)} records. ") %>% message(appendLF = FALSE)

  # local TZ conversion
  if (!is.null(convert_to_TZ) && "log_datetime" %in% names(logs)) {
    if (!quiet) glue::glue("Converting to timezone '{convert_to_TZ}'.") %>%
      message(appendLF = FALSE)
    logs <- mutate(logs, log_datetime = with_tz(log_datetime, convert_to_TZ))
  }
  message("")

  return(logs)
}
