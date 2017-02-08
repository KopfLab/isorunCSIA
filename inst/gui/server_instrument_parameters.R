### INSTRUMENT PARAMETERS
instrument_table <- callModule(
  historyInfoTable, "parameters", parameters = parameters, history_files = history_files,
  mode_input = reactive(input$mode), clear_input = reactive(input$instrument_new_clear),
  number_format = "0.000")
