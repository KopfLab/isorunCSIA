#' LINEARITY and on/off calibration

lin_on_off_files <- callModule(fileSelector, "lin_on_off_files", pattern = "(on off|on_off|linearity)\\.dxf$",
                               root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                               number_recent = 20, sort_desc = TRUE,
                               exclude_recent = INSTRUMENT_HISTORY_FOLDER)

# on/offs table
on_offs_table <- callModule(serverDataTableSimple, "on_offs_table")

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

    if(!is.null(values$on_off_data_table)) {
      # summarise data
      values$on_off_data_table <-
        values$on_off_data_table %>%
          mutate(`ON/OFF file` = sub("^((MAT)?\\d+).*", "\\1", File)) %>%
          group_by(`ON/OFF file`) %>%
          generate_summary_table(fun = funs(average = mean, sigma = sd),
                                 cols = c("A44"="Ampl 44", "Ampl 45"),
                                 n_col = "# peaks", col_fun_sep = " - ")
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


