#' LINEARITY and on/off calibration

# relevant settings
lin_on_off_deltas <- get_setting(global, "delta_col") %>% vectorize_setting_string()
lin_on_off_amps <- get_setting(global, "amp_col") %>% vectorize_setting_string()
on_off_table_cols <- c(lin_on_off_deltas, lin_on_off_amps)
on_off_file_pattern <- get_setting(global, "on_off_file_pattern")
linearity_file_pattern <- get_setting(global, "linearity_file_pattern")

# linearity on/off file selector
lin_on_off_files <- callModule(fileSelector, "lin_on_off_files",
                               pattern = paste0("(", on_off_file_pattern, "|", linearity_file_pattern, ")"),
                               root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                               number_recent = 20, sort_desc = TRUE,
                               exclude_recent = INSTRUMENT_HISTORY_FOLDER)

# on/offs table
on_offs_table <- callModule(serverDataTableSimple, "on_offs_table", sig_digits = 2)

# load linearity and on/off as soon as files are selected
observe({
  lin_on_off_files$modal_closed()
  isolate({
    message("INFO: loading linearity and on/offs")
    values$lin_on_off_objects <- load_iso_data(lin_on_off_files$selection_relative(),
                                               loaded = values$lin_on_off_objects, root = data_dir)
    values$lin_data_table <- get_iso_data_tables(
      values$lin_on_off_objects[grepl("linearity", names(values$lin_on_off_objects), ignore.case = TRUE) ])

    values$on_off_data_table <- get_iso_data_tables(
      values$lin_on_off_objects[grepl("(on_off|on off)", names(values$lin_on_off_objects), ignore.case = TRUE) ])

    if(!is.null(values$on_off_data_table) && !is.null(on_off_table_cols)) {
      # check if any of the requested summarize columns are missing
      if ( length(missing <- setdiff(on_off_table_cols, names(values$on_off_data_table))) > 0 ) {
        message("ERROR: cannot load on/off table because the following columns from the settings do not exist: ", paste(missing, collapse=", "))
        values$on_off_data_table <- NULL
      } else {
        # summarise data
        remove_delta_cols <- c(1:length(lin_on_off_deltas))*2+1
        values$on_off_data_table <-
          values$on_off_data_table %>%
          mutate(`ON/OFF file` = sub("^((MAT)?\\d+).*", "\\1", File)) %>%
          group_by(`ON/OFF file`) %>%
          generate_summary_table(fun = funs(avg = mean, stdev = sd),
                                 cols = on_off_table_cols,
                                 n_col = "# peaks", col_fun_sep = ": ") %>%
          select(-remove_delta_cols)
      }
    }

  })
})

# update and show/hide the on offs table
observe({
  if (is.null(values$on_off_data_table)) {
    shinyjs::hide("on_offs_div")
  } else {
    on_offs_table$update(values$on_off_data_table)
    shinyjs::show("on_offs_div")
  }
})


