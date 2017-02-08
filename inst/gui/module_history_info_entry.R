#---- CLIENT SIDE

#' the history info input table and notes field
historyInfoInput <- function(id, caption = id) {
  ns <- NS(id)
  tagList(
    rHandsontableOutput(ns("hot")),
    textAreaInput(ns("notes"), label = NULL, placeholder = sprintf("Enter any additional notes on the %s", caption), height = "50px", width = "400px")
  )
}

historyArchiveButton <- function(id, caption = id, wrapper = h4) {
  ns <- NS(id)
  tagList(
    wrapper(actionLink(ns("archive"), sprintf("Record %s information", caption), icon = icon("archive")), textOutput(ns("archive_status"), inline=TRUE))
  )
}

#---- SERVER SIDE
#' @param number_format how to format numbers in the table, see http://numeraljs.com/ for details
historyInfoTable <- function(input, output, session,
                             parameters, history_files,
                             mode_input, clear_input,
                             number_format = "0") {

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
    isolate(values$saved[mode_input()] <- TRUE)
    data <- isolate({
      bind_cols(
        # add timestamp
        data_frame(timestamp = Sys.time() %>% format()),
        # settings
        values$hot[[mode_input()]] %>%
          left_join(select(element_parameters[[mode_input()]], Column, Caption, Type), by = "Caption") %>%
          mutate(save_value = ifelse(Type == "bool", as.numeric(Check), Value)) %>%
          select(Column, save_value) %>% spread(Column, save_value),
        # notes
        data_frame(Notes = input$notes))
    })

    # check for consistency with previous history files
    ele_history_file <- filter(history_files, Element == isolate(mode_input()), Category == ns(NULL))$filepath[1]
    if (length(ele_history_file) == 0) stop("can't find history file for this element and parameter category")
    message("INFO: saving '", ns(NULL), "' parameters for ", isolate(mode_input()), " in ", ele_history_file)
    if (file.exists(ele_history_file)) {
      check_headers <- read.csv(file = ele_history_file, header = TRUE, nrows = 1, stringsAsFactors = FALSE)
      if (ncol(check_headers) != ncol(data) || !all(names(check_headers) == names(data))) {
        old_data <- read.csv(file = ele_history_file, header = TRUE, stringsAsFactors = FALSE)
        old_ele_history_file <- sub("\\.csv", paste0("_deprecated_", Sys.time() %>% format("%Y%m%d_%H%M%S"), ".csv"), ele_history_file)
        message("WARNING: headers in history file don't match new data set --> deprecating history file to ", basename(old_ele_history_file))
        file.rename(ele_history_file, old_ele_history_file)
        write.table(data, file = ele_history_file, row.names = FALSE, sep = ",", col.names = TRUE)
      } else {
        write.table(data, file = ele_history_file, row.names = FALSE, sep = ",", append = TRUE, col.names = FALSE)
      }
    } else {
      write.table(data, file = ele_history_file, row.names = FALSE, sep = ",", col.names = TRUE)
    }

  })

  # status message
  output$archive_status <- renderText({
    validate(need(!is.null(values$saved), message = "no saved information"),
             need(!is.null(values$saved[mode_input()]), message = "no element-specific data saved"))
    if (values$saved[mode_input()])
      return(Sys.time() %>% format(" (saved at %H:%m:%S on %d %b %Y)"))
    else
      return("")
  })

  # store hot data in reactive values
  observe({
    validate(need(input$hot, message = "no parameters"))
   isolate(values$hot[[mode_input()]] <-  suppressWarnings(hot_to_r(input$hot)))
  })

  # render handsontable
  output$hot = renderRHandsontable({
    # trigger based on new init_data and mode_input()
    init_data()
    hot <- isolate(values$hot)

    validate(
      need(mode_input(), message = "no element selected"), # switch element
      need(hot, message = "no parameters"),
      need(hot[[mode_input()]], message = "no element-specific parameters"))

    message("INFO: generating '", ns(NULL), "' table for element ", mode_input())

    # generate handsontable
    hot[[mode_input()]] %>%
      select(Mode, Caption, Check, Value, Units, Info) %>%
      rhandsontable() %>%
      hot_table(readOnly = TRUE, highlightRow = TRUE, columnSorting = FALSE, contextMenu = FALSE,
                customBorders = generate_borders(hot[[mode_input()]]$Type)) %>%
      hot_col(col = "Value", readOnly = FALSE, format = number_format) %>%
      hot_col(col = "Check", readOnly = FALSE)
  })


  # return both the current path and the selected folder contents
  list(
    hot = reactive(values$hot),
    archive = reactive(input$archive),
    saved = reactive(values$saved),
    notes = reactive(input$notes)
  )
}

#---- UTILS
#' function to generate the list structure for
#' the borders of the parameter tables
generate_borders <- function(type, type_col = c("bool" = 1, "numeric" = 2), width = 2, color = "green") {
  border <- list(width = width, color = color)
  borders <- list(top = border, left = border, bottom = border, right = border)
  lapply(seq(0, length(type)-1)[type %in% names(type_col)], function(row) {
    modifyList(list(row = row, col = type_col[type[row+1]] %>% as.integer()), borders)
  })
}

