# fields =======

#' Retrieve instrument log fields
#'
#' @inheritParams csia_get_instrument_logs
#' @param category_id which catgories to include
#' @export
csia_get_instrument_log_fields <- function(
  instrument_id = default(instrument_id), mode_id = NULL, category_id = NULL, filter = NULL,
  select = everything(), con = default(con), quiet = default(quiet)
) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  instrument_id_value <- instrument_id

  if (!quiet) {
    glue::glue("Info: retrieving instrument log fields for '{instrument_id_value}'",
               if (!is.null(mode_id)) " in {paste(mode_id, collapse = ', ')} mode" else " in all modes",
               "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}... ") %>%
      message(appendLF = FALSE)
  }

  log_fields <-
    tbl(con, "instrument_log_fields") %>%
    left_join(tbl(con, "instrument_mode_log_fields"), by = c("instrument_id", "field_id")) %>%
    left_join(tbl(con, "instrument_log_categories"), by = c("instrument_id", "category_id")) %>%
    arrange(category_sorting, field_sorting) %>%
    dplyr::filter(instrument_id == instrument_id_value) %>%
    {
      if(!is_null(mode_id)) dplyr::filter(., mode_id %in% !!mode_id)
      else .
    } %>%
    {
      if(!is_null(category_id)) dplyr::filter(., category_id %in% !!category_id)
      else .
    } %>%
    {
      if(!quo_is_null(filter_quo)) dplyr::filter(., !!filter_quo)
      else .
    } %>%
    dplyr::select(!!select_quo) %>%
    collect()


  if (!quiet) glue::glue("found {nrow(log_fields)} records. ") %>% message(appendLF = FALSE)

  return(log_fields)
}


# logs ====

#' Retrieve instrumnet logs
#'
#' Returns instrument logs joined with sessions, instrument modes and instrument fields. Sorted by descending order (i.e. latest record first).
#'
#' @param instrument_id which instrument to run this function for
#' @param mode_id which mode(s) to run this function for. If undefined, implies all modes.
#' @param session_id which session(s) to get logs for. If undefined, get logs for all sessions.
#' @param filter what filter conditions to apply, if any (forwarded to \link[dplyr]{filter}). Can be based on any of the joined tables.
#' @param select what columns to select (forwarded to \link[select]{select})
#' @param max_rows if provided, only selects the indicated number of rows (more efficient this way than part of the filter)
#' @param convert_to_TZ if provided, converts the log_datetime to the provided timezone (by default the local one stored in \code{Sys.timezone()}). If NULL, will keep it as UTC.
#' @return instrument logs
#' @export
csia_get_instrument_logs <- function(
  instrument_id = default(instrument_id),
  mode_id = NULL, session_id = NULL, filter = NULL,
  select = everything(),
  max_rows = NULL,
  convert_to_TZ = Sys.timezone(),
  con = default(con), quiet = default(quiet)) {

  con <- validate_db_connection(enquo(con))
  filter_quo <- enquo(filter)
  select_quo <- enquo(select)
  instrument_id_value <- instrument_id

  if (!quiet) {
    glue::glue("Info: retrieving instrument logs for '{instrument_id_value}'",
         if (!is.null(mode_id)) " in {paste(mode_id, collapse = ', ')} mode" else " in all modes",
         if (!is.null(session_id)) " for session {paste(session_id, collapse = ', ')}" else " for all sessions",
         "{if(!quo_is_null(filter_quo)) str_c(\" with filter '\", quo_text(filter_quo), \"'\") else ''}",
         "{if(!is.null(max_rows)) str_c(\", limited to \", max_rows, \" rows max\") else ''}... ") %>%
    message(appendLF = FALSE)
  }

  logs <- tbl(con, "instrument_logs") %>%
    left_join(tbl(con, "instrument_log_fields"), by = c("instrument_id", "field_id")) %>%
    left_join(tbl(con, "instrument_modes"), by = c("instrument_id", "mode_id")) %>%
    left_join(tbl(con, "sessions"), by = c("instrument_id", "session_id", "mode_id")) %>%
    arrange(desc(log_id)) %>%
    dplyr::filter(instrument_id == instrument_id_value) %>%
    {
      if(!is_null(mode_id)) dplyr::filter(., mode_id %in% !!mode_id)
      else .
    } %>%
    {
      if(!is_null(session_id)) dplyr::filter(., session_id %in% !!mode_id)
      else .
    } %>%
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
  if (!is.null(convert_to_TZ) && "datetime" %in% names(logs)) {
    if (!quiet) glue::glue("Converting to timezone '{convert_to_TZ}'.") %>%
      message(appendLF = FALSE)
    logs <- mutate(logs, datetime = with_tz(datetime, convert_to_TZ))
  }
  message("")

  return(logs)
}
