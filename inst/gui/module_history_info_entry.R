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
                             modes, parameters,
                             mode_input, user_input, clear_input,
                             number_format = "0") {

  # namespace
  ns <- session$ns

  # element parameters
  mode_parameters <- list()
  for (mode in modes$Mode) {
    mode_parameters[[mode]] <- parameters %>%
      filter(Category == ns(NULL), Mode == "all" | grepl(mode, Mode, fixed=TRUE)) %>%
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
    values$saved <- rep(FALSE, nrow(modes)) %>% setNames(modes$Mode)
    for (mode in modes$Mode) {
      isolate(values$hot[[mode]] <- mode_parameters[[mode]])
    }
    updateTextAreaInput(session, "notes", value = "")
  })

  # archive data
  observe({
    validate(need(input$archive, message = FALSE))
    isolate(values$saved[mode_input()] <- TRUE) # store information the this mode was saved

    # find data
    data <- isolate({
      bind_cols(
        # add user, mode, timestamp and notes
        data_frame(
          timestamp = format(Sys.time()),
          user = user_input(),
          mode = mode_input(),
          notes = input$notes
        ),
        # add the settings
        values$hot[[mode_input()]] %>%
          left_join(select(mode_parameters[[mode_input()]], Column, Caption, Type), by = "Caption") %>%
          mutate(save_value = ifelse(Type == "bool", as.numeric(Check), Value)) %>%
          select(Column, save_value) %>% spread(Column, save_value)
        )
    })

    # add the other fields for this history type (that are not included in the mode)
    missing_cols <- parameters %>% filter(Category == ns(NULL), !Column %in% names(data))
    if (nrow(missing_cols) > 0) {
      data <- bind_cols(
        data,
        missing_cols %>% select(Column) %>% distinct() %>% mutate(value = NA_real_) %>% spread(Column, value)
      )
    }

    # arrange as listed in setup
    data <- data[c("timestamp", "user", "mode", unique(filter(parameters, Category == ns(NULL))$Column), "notes")]

    # save
    history_file <- HISTORY_FILES[ns(NULL)]
    message("INFO: saving '", ns(NULL), "' parameters in ", history_file)

    if (!file.exists(history_file)) {
      # new file
      write.table(data, file = history_file, row.names = FALSE, sep = ",", col.names = TRUE)
    } else {
      # existing file, make sure headers are compatible
      check_headers <- read.csv(file = history_file, header = TRUE, nrows = 1, stringsAsFactors = FALSE)
      if (!identical(names(data), names(check_headers))) {
        # header mismatch - merge files (discard old file information no longer in the history files)
        old_history_file <- sub("\\.csv", paste0("_backup_", Sys.time() %>% format("%Y%m%d_%H%M%S"), ".csv"), history_file)
        message("WARNING: header mismatch with existing file (missing: ",
                paste(setdiff(names(data), names(check_headers)), collapse = ", "),
                "; extra: ", paste(setdiff(names(check_headers), names(data)), collapse = ", "),
                ") - merging information and creating backup history file at ", basename(old_history_file))

        # make backup copy
        file.copy(history_file, old_history_file)

        # save combined data
        combined_data <- bind_rows(
          data,
          read.csv(file = history_file, header = TRUE, stringsAsFactors = FALSE)
        ) %>% arrange(timestamp)
        write.table(combined_data[names(data)], file = history_file, row.names = FALSE, sep = ",", col.names = TRUE)
      } else {
        # append to existing file
        write.table(data, file = history_file, row.names = FALSE, sep = ",", append = TRUE, col.names = FALSE)
      }
    }

  })

  # status message
  output$archive_status <- renderText({
    validate(need(!is.null(values$saved), message = "no saved information"),
             need(!is.null(values$saved[mode_input()]), message = "no element-specific data saved"))
    if (values$saved[mode_input()])
      return(Sys.time() %>% format(" (saved at %H:%M:%S on %d %b %Y)"))
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
      select(Caption, Check, Value, Units, Mode, Info) %>%
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

