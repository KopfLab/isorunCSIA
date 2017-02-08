### INSTRUMENT PARAMETERS
instrument_table <- callModule(
  historyInfoTable, "parameters", modes = modes, parameters = parameters,
  mode_input = reactive(input$mode), user_input = reactive(input$user), clear_input = reactive(input$instrument_new_clear),
  number_format = "0.000")
