### SENSITVITY & PEAK SHAPE
sensitivity_table <- callModule(
  historyInfoTable, "sensitivity", parameters = parameters, history_files = history_files,
  mode_input = reactive(input$mode), clear_input = reactive(input$instrument_new_clear))
peak_shape_files <- callModule(fileSelector, "peak_shape_files", pattern = "\\.scn$",
                               root = data_dir, root_name = "All", size = 12, multiple = FALSE,
                               number_recent = 10, exclude_recent = INSTRUMENT_HISTORY_FOLDER)

# save peak shape file name when modal is closed
observe({
  peak_shape_files$modal_closed()
  isolate({
    file <- peak_shape_files$selection_relative()
    if ( length(file) > 0 && grepl("\\.scn$",file)) {
      values$peak_shape_file <- file
      message("Saving peak shape file ", file)
      file.copy(from = file.path(data_dir, file),
                to = file.path(data_dir, PEAK_SHAPE_FOLDER, sprintf("%s_%s_peak_shape.scn", format(Sys.time(), "%Y%m%d_%H%M%S"), isolate(input$mode))))
    }
  })
})
# show peak shape file name
output$peak_shape_file <- renderText({
  if (is.null(values$peak_shape_file)) ""
  else sprintf("Saved %s at %s", values$peak_shape_file, format(Sys.time()))
})
