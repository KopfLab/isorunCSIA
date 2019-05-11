# log field ====

#' Add new instrument log fields
#'
#' Adds a new instrument log field to an instrument and category for one or multiple modes. Note that this will always add the new log field with the new, highest sorting number so this command should be called for new log fields in the order that they are supposed to be sorted.
#'
#' @inheritParams csia_get_instrument_logs
#' @param category_id which category does the new log field belong too? Has to be a single value.
#' @param mode_id in which modes is this log field active?
#' @param field_caption what is the caption of this field? should be informative short text
#' @param field_type what type is this field? must be one of the existing values in instrument_log_field_types (e.g. 'checkbox' or 'numeric')
#' @param field_unit the field unit [optional]
#' @param field_information instructions for how to fill out information for this field [optional but highly recommended]
#' @export
csia_add_instrument_log_field <- function(instrument_id = default(instrument_id), category_id, mode_id, field_caption, field_type, field_unit = NA_character_, field_information = NA_character_, con = default(con), quiet = default(quiet)) {

  # safety checks
  stopifnot(length(category_id) == 1)
  stopifnot(length(mode_id) >= 1)
  con <- validate_db_connection(enquo(con))

  if (!quiet) {
    glue::glue(
      "Info: add new instrument log field '{field_caption}' in category '{category_id}' ",
      "for '{instrument_id}'... ") %>%
      message(appendLF = FALSE)
  }
  data <- tibble(instrument_id, category_id, field_caption, field_type, field_unit, field_information)
  result <- run_insert_sql(data, "instrument_log_fields", con, quiet = TRUE)

  # figure out ID and other new info
  if (result == 1) {
    data <-
      tbl(con, "instrument_log_fields") %>%
      dplyr::arrange(desc(field_id)) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::collect()
  }

  # info
  if (!quiet) {
    glue::glue("created {result} new log field",
               if (result == 1) " with new field_id={data$field_id}."
               else " but that's not correct, something must have gone wrong.") %>%
      message(appendLF = FALSE)
  }

  # modes
  if (result == 1) {
    if (!quiet) {
      glue::glue(" Adding mode log field records for mode(s) '{paste(mode_id, collapse = \"', '\")}'... ") %>%
        message(appendLF = FALSE)
    }
    modes_data <- tibble(field_id = data$field_id, instrument_id, mode_id)
    result <- run_insert_sql(modes_data, "instrument_mode_log_fields", con, quiet = TRUE)
    if (!quiet) glue::glue("created {result} new entries.") %>% message(appendLF = FALSE)
  }
  message()

  return(invisible(data))
}

