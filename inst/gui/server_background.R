### BACKGROUND
background_table <- callModule(
  historyInfoTable, "background", parameters = parameters, history_files = history_files,
  mode_input = reactive(input$mode), clear_input = reactive(input$instrument_new_clear))
full_scan_files <- callModule(fileSelector, "full_scan_files", pattern = "\\.scn$",
                              root = data_dir, root_name = "All", size = 12, multiple = FALSE,
                              number_recent = 10, exclude_recent = INSTRUMENT_HISTORY_FOLDER)


# save full scan file when modal is closed
observe({
  full_scan_files$modal_closed()
  isolate({
    file <- full_scan_files$selection_relative()
    if ( length(file) > 0 && grepl("\\.scn$",file)) {
      values$full_scan_file <- file
      message("Saving full scan file ", file)
      file.copy(from = file.path(data_dir, file),
                to = file.path(data_dir, FULL_SCAN_FOLDER, sprintf("%s_full_scan.scn", format(Sys.time(), "%Y%m%d_%H%M%S"))))
    }
  })
})
# show full scan file name
output$full_scan_file <- renderText({
  if (is.null(values$full_scan_file)) ""
  else sprintf("Saved %s at %s", values$full_scan_file, format(Sys.time()))
})
