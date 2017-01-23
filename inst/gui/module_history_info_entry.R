#---- CLIENT SIDE

#' the history info input table and notes field
historyInfoInput <- function(id, caption = id) {
  ns <- NS(id)
  tagList(
    rHandsontableOutput(ns("hot")),
    textAreaInput(ns("notes"), label = NULL, placeholder = sprintf("Enter any additional notes on the %s", caption), height = "50px", width = "400px")
  )
}

historyArchiveButton <- function(id, caption = id) {
  ns <- NS(id)
  tagList(
    h4(actionLink(ns("archive"), sprintf("Record %s information", caption), icon = icon("archive")), textOutput(ns("archive_status"), inline=TRUE))
  )
}

#---- SERVER SIDE
historyInfoTable <- function(input, output, session,
                             parameters, element_input, clear_input,
                             history_file) {

  # namespace
  ns <- session$ns

  # element parameters
  element_parameters <- list()
  for (element in ELEMENTS) {
    element_parameters[[element]] <- parameters %>%
      filter(Category == ns(NULL), Element == "all" | grepl(element, Element, fixed=TRUE)) %>%
      mutate(Check = FALSE, Value = NA_real_)
  }

  # reactive values
  values <- reactiveValues(
    hot = NULL,
    saved = NULL
  )

  # initialize data
  init_data <- reactive({
    clear_input() # trigger reset
    message("INFO: initializing data for '", ns(NULL), "'") # debug

    # values
    values$hot <- list()
    values$saved <- rep(FALSE, length(ELEMENTS)) %>% setNames(ELEMENTS)
    for (element in ELEMENTS) {
      isolate(values$hot[[element]] <- element_parameters[[element]])
    }
    updateTextAreaInput(session, "notes", value = "")
  })

  # archive data
  observe({
    validate(need(input$archive, message = FALSE))
    isolate(values$saved[element_input()] <- TRUE)
    data <- isolate({
      bind_cols(
        # add timestamp
        data_frame(timestamp = Sys.time() %>% format()),
        # settings
        values$hot[[element_input()]] %>%
          left_join(select(element_parameters[[element_input()]], Column, Caption, Type), by = "Caption") %>%
          mutate(save_value = ifelse(Type == "bool", as.numeric(Check), Value)) %>%
          select(Column, save_value) %>% spread(Column, save_value),
        # notes
        data_frame(Notes = input$notes))
    })

    ele_history_file <- file.path(dirname(history_file), paste0(isolate(element_input()), "_", basename(history_file)))
    message("INFO: saving '", ns(NULL), "' parameters for ", isolate(element_input()), " in ", ele_history_file)
    if (file.exists(ele_history_file)) {
      write.table(data, file = ele_history_file, row.names = FALSE, sep = ",", append = TRUE, col.names = FALSE)
    } else {
      write.table(data, file = ele_history_file, row.names = FALSE, sep = ",", col.names = TRUE)
    }

  })

  # status message
  output$archive_status <- renderText({
    validate(need(!is.null(values$saved), message = "no saved information"),
             need(!is.null(values$saved[element_input()]), message = "no element-specific data saved"))
    if (values$saved[element_input()])
      return(Sys.time() %>% format(" (saved at %H:%m:%S on %d %b %Y)"))
    else
      return("")
  })

  # store hot data in reactive values
  observe({
    validate(need(input$hot, message = "no parameters"))
    isolate(values$hot[[element_input()]] <- hot_to_r(input$hot))
  })

  # render handsontable
  output$hot = renderRHandsontable({
    # trigger based on new init_data and element_input()
    init_data()
    hot <- isolate(values$hot)

    validate(
      need(element_input(), message = "no element selected"), # switch element
      need(hot, message = "no parameters"),
      need(hot[[element_input()]], message = "no element-specific parameters"))

    message("INFO: generating '", ns(NULL), "' table for element ", element_input()) # debug

    # generate handsontable
    hot[[element_input()]] %>%
      select(Caption, Check, Value, Units, Info) %>%
      rhandsontable() %>%
      hot_table(readOnly = TRUE, highlightRow = TRUE, columnSorting = FALSE, contextMenu = FALSE,
                customBorders = generate_borders(hot[[element_input()]]$Type)) %>%
      hot_col(col = "Value", readOnly = FALSE, format = "0") %>%
      hot_col(col = "Check", readOnly = FALSE)
  })


  # return both the current path and the selected folder contents
  list(
    hot = reactive(values$hot),
    saved = reactive(values$saved),
    notes = reactive(input$notes)
  )
}
