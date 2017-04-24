#' LINEARITY and on/off calibration

# relevant reactive values
values$lin_on_off_objects <- list()
values$lin_data_table <- list()
values$on_off_data_table <- list()

# relevant settings
on_off_file_pattern <- get_setting(global, "on_off_file_pattern")
linearity_file_pattern <- get_setting(global, "linearity_file_pattern")
lin_on_off_deltas <- reactive({req(input$mode); get_setting(global, "delta_col", mode = input$mode) %>% vectorize_setting_string()})
lin_on_off_amps <- reactive({req(input$mode); get_setting(global, "amp_col", mode = input$mode) %>% vectorize_setting_string()})

# linearity on/off file selector
lin_on_off_files <- callModule(fileSelector, "lin_on_off_files",
                               pattern = paste0("(", on_off_file_pattern, "|", linearity_file_pattern, ")"),
                               root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                               number_recent = 20, sort_desc = TRUE,
                               exclude_recent = INSTRUMENT_HISTORY_FOLDER)

# on/offs table
on_offs_table <- callModule(serverDataTableSimple, "on_offs_table", sig_digits = 2)

# load linearity and on/off as soon as files are selected (input mode dependent)
observe({
  lin_on_off_files$modal_closed()
  isolate({
    req(input$mode) # make sure input$mode is set
    message("INFO: loading linearity and on/offs")
    values$lin_on_off_objects[[input$mode]] <- load_iso_data(lin_on_off_files$selection_relative(),
                                               loaded = values$lin_on_off_objects[[input$mode]], root = data_dir)

    values$lin_data_table[[input$mode]] <- get_iso_data_tables(
      values$lin_on_off_objects[[input$mode]][grepl(linearity_file_pattern, names(values$lin_on_off_objects[[input$mode]]), ignore.case = TRUE) ])

    values$on_off_data_table[[input$mode]] <- get_iso_data_tables(
      values$lin_on_off_objects[[input$mode]][grepl(on_off_file_pattern, names(values$lin_on_off_objects[[input$mode]]), ignore.case = TRUE) ])

    # generate table summary
    on_off_table_cols <- c(lin_on_off_deltas(), lin_on_off_amps())
    if(!is.null(values$on_off_data_table[[input$mode]]) && !is.null(on_off_table_cols)) {
      # check if any of the requested summarize columns are missing
      if ( length(missing <- setdiff(on_off_table_cols, names(values$on_off_data_table[[input$mode]]))) > 0 ) {
        message("ERROR: cannot load on/off table because the following columns from the settings do not exist: ", paste(missing, collapse=", "))
        values$on_off_data_table[[input$mode]] <- NULL
      } else {
        # summarise data
        remove_delta_cols <- c(1:length(lin_on_off_deltas()))*2+1
        values$on_off_data_table[[input$mode]] <-
          values$on_off_data_table[[input$mode]] %>%
          mutate(`ON/OFF file` = sub("^((MAT)?\\d+).*", "FF\\1", File)) %>%
          group_by(`ON/OFF file`) %>%
          generate_summary_table(fun = funs(avg = mean, stdev = sd),
                                 cols = on_off_table_cols,
                                 n_col = "# peaks", col_fun_sep = ": ",
                                 summary_row = "All files") %>%
          select(-remove_delta_cols)
      }
    }
  })
})

# update and show/hide the on offs table
observe({
  req(input$mode)
  if (is.null(values$on_off_data_table[[input$mode]])) {
    shinyjs::hide("on_offs_div")
  } else {
    on_offs_table$update(values$on_off_data_table[[input$mode]])
    shinyjs::show("on_offs_div")
  }
})


