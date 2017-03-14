### TUNING
tuning_table <- callModule(
  historyInfoTable, "tuning", modes = modes, parameters = parameters,
  mode_input = reactive(input$mode), user_input = reactive(input$user), clear_input = reactive(input$tuning_new_clear),
  number_format = "0.000")
tuning_peak_shape_files <- callModule(fileSelector, "tuning_peak_shape_files", pattern = "\\.scn$",
                               root = data_dir, root_name = "All", size = 12, multiple = FALSE,
                               number_recent = 20, sort_desc = TRUE,
                               exclude_recent = INSTRUMENT_HISTORY_FOLDER)

# save peak shape file name when modal is closed
observe({
  tuning_peak_shape_files$modal_closed()
  isolate({
    file <- tuning_peak_shape_files$selection_relative()
    if ( length(file) > 0 && grepl("\\.scn$",file)) {
      values$tuning_peak_shape_file <- file
      message("Saving tuning peak shape file ", file)
      file.copy(from = file.path(data_dir, file),
                to = file.path(data_dir, PEAK_SHAPE_FOLDER, sprintf("%s_%s_peak_shape.scn", format(Sys.time(), "%Y%m%d_%H%M%S"), isolate(input$mode))))
    }
  })
})
# show peak shape file name
output$tuning_peak_shape_file <- renderText({
  if (is.null(values$tuning_peak_shape_file)) ""
  else sprintf("(saved %s at %s)", values$tuning_peak_shape_file, format(Sys.time(), "%H:%M:%S on %d %b %Y"))
})
