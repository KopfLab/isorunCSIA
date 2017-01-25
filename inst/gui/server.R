

# SERVER =====
server <- shinyServer(function(input, output, session) {

  # STARTUP =======
  data_dir <- .GlobalEnv$.base_dir
  if (!dir.exists(data_dir)) dir.create(data_dir)
  if (!dir.exists(file.path(data_dir, INSTRUMENT_HISTORY_FOLDER))) dir.create(file.path(data_dir, INSTRUMENT_HISTORY_FOLDER))

  message("\n***************************************************************",
          "\nINFO: Launching GUI ...",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", data_dir,
          "\nINFO: Settings file: ", file.path(data_dir, SETTINGS_FILE),
          "\nINFO: History folder: ", file.path(data_dir, INSTRUMENT_HISTORY_FOLDER))

  # SETTINGS =======
  settings <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "global")
  parameters <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "parameters")
  history_files <- expand.grid(Element = ELEMENTS, Category = names(HISTORY_FILES)) %>%
    mutate(
      filename = HISTORY_FILES[Category],
      filepath = file.path(data_dir, INSTRUMENT_HISTORY_FOLDER, paste0(Element, "_", filename))
    )

  # REACTIVE VALUES ----
  values <- reactiveValues(
    tuning_file_preview = NULL,
    full_scan_file = NULL,
    peak_shape_file = NULL,
    history_variables = c()
  )

  # INSTRUMENT NEW PARAMETER RECORD ----
  observe({ input$instrument_new_clear; values$full_scan_file <- NULL; values$peak_shape_file <- NULL }) # reset
  ### BACKGROUND
  background_table <- callModule(
    historyInfoTable, "background", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear))
  full_scan_files <- callModule(fileSelector, "full_scan_files", pattern = "\\.scn$",
                                root = data_dir, root_name = "Data", size = 12, multiple = FALSE)
  observe({
    validate(need(full_scan_files$selection(), message = FALSE) %then%
               need(length(full_scan_files$selection() == 1), message = FALSE) %then%
               need(grepl("\\.scn$", basename(full_scan_files$selection())), message = FALSE))
    isolate(values$full_scan_file <- full_scan_files$selection())
  })
  output$full_scan_file <- renderText({
    if (is.null(values$full_scan_file)) "No file selected"
    else sub(data_dir, "", values$full_scan_file, fixed = TRUE)
  })
  observe({ # save full scan file
    background_table$archive()
    scn_file <- isolate(values$full_scan_file)
    message("Saving full scan file ", scn_file)
    if (!is.null(scn_file)) {
      file.copy(from = scn_file,
                to = file.path(data_dir, FULL_SCAN_FOLDER, paste0(Sys.time() %>% format("%Y%m%d_%H%M%S"), "_full_scan.scn")))
    }
    isolate(values$full_scan_file <- NULL)
  })

  ### SENSITVITY & PEAK SHAPE
  sensitivity_table <- callModule(
    historyInfoTable, "sensitivity", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear))
  peak_shape_files <- callModule(fileSelector, "peak_shape_files", pattern = "\\.scn$",
                                root = data_dir, root_name = "Data", size = 12, multiple = FALSE)
  observe({
    validate(need(peak_shape_files$selection(), message = FALSE) %then%
               need(length(peak_shape_files$selection() == 1), message = FALSE) %then%
               need(grepl("\\.scn$", basename(peak_shape_files$selection())), message = FALSE))
    isolate(values$peak_shape_file <- peak_shape_files$selection())
  })
  output$peak_shape_file <- renderText({
    if (is.null(values$peak_shape_file)) "No file selected"
    else sub(data_dir, "", values$peak_shape_file, fixed = TRUE)
  })
  observe({ # save peak shape file
    sensitivity_table$archive()
    scn_file <- isolate(values$peak_shape_file)
    message("Saving peak shape file ", scn_file)
    if (!is.null(scn_file)) {
      file.copy(from = scn_file,
                to = file.path(data_dir, PEAK_SHAPE_FOLDER, paste0(Sys.time() %>% format("%Y%m%d_%H%M%S"), "_peak_shape.scn")))
    }
    isolate(values$peak_shape_file <- NULL)
  })

  ### INSTRUMENT PARAMETERS
  instrument_table <- callModule(
    historyInfoTable, "parameters", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear),
    number_format = "0.000")


  # PARAMETER HISTORY ----
  get_history_data <- reactivePoll(3000, session,
    # checks every second whether any of the history files were modified
    checkFunc = function() {
      sapply(history_files$filepath, function(path) {
        if(file.exists(path)) file.info(path)$mtime[1] else ""
      }) %>% paste(collapse = "|")
    },
    # reads the log files
    valueFunc = function() {
      message("INFO: re-loading history files")
      files <-
        history_files %>% group_by(Element, Category, filepath) %>%
        do({
          if (file.exists(.$filepath[1])) {
            read.csv(.$filepath[1], header = TRUE, stringsAsFactors = FALSE,
                     row.names = NULL, colClasses = c(timestamp = "POSIXct", Notes = "character")) %>%
              gather(Column, value, -timestamp, -Notes)
          } else {
            data_frame()
          }
        }) %>%
        ungroup()
      if (nrow(files) == 0) return(NULL)
      else {
        files %>%
        left_join(
          rename(parameters, Element_def = Element),
          by = c("Category", "Column")) %>%
        mutate(date = as.Date(timestamp, tz = format(timestamp[1], "%Z"))) %>%
        select(-filepath) %>% filter(!is.na(Type))
      }
    }
  )

  # update the variable selection box and date range
  observe({
    validate(need(get_history_data(), message = FALSE))
    history_vars <- get_history_data() %>%
      filter(Type == "numeric") %>%  # only numerics
      filter(Element %in% input$history_element) %>%
      filter(Category %in% input$history_category) %>%
      mutate(Label = paste0(Caption, " [", Units, "]: ", Element_def, "")) %>%
      select(Column, Label) %>%
      distinct() %>% arrange(Column)
    options <- history_vars$Column %>% setNames(history_vars$Label)
    selected <- options[options %in% isolate(values$history_variables)]
    updateSelectInput(session, "history_variables",
                      choices = history_vars$Column %>% setNames(history_vars$Label),
                      selected = selected)

    time_range <- range(get_history_data()$date)
    updateDateRangeInput(session, "history_date_range",
                         min = time_range[1], max = time_range[2],
                         start = time_range[1], end = time_range[2])
  })

  # save the selected rows in the reactive values
  observe({
    validate(need(input$history_variables, message = FALSE))
    selected_variables <- input$history_variables
    message("INFO: storing history parameter selection: '", paste(selected_variables, collapse = ", "), "'") # debug
    isolate(values$history_variables <- selected_variables)
  })

  get_history_plot <- reactive({
    message("INFO: generating history plot")
    validate(
      need(input$history_variables, message = "Please select parameter(s) to display history plot.") %then%
      need(input$history_date_range, message = "Please select a non-zero date range.") %then%
      need(nrow(data <- get_history_data() %>%
                  filter(Column %in% input$history_variables,
                         date >= input$history_date_range[1],
                         date <= input$history_date_range[2])) > 0,
           message = "No data available for these parameter(s) in this date range."),
      errorClass = "info"
      )
    data %>%
    mutate(Date = timestamp, Value = value, Panel = paste(Caption, Units)) %>%
    ggplot() + aes(x = Date, y = Value, color = Element, text = paste0(Panel, "<br>Notes: ", Notes)) +
      geom_line(linetype = 1) +
      geom_point(size = 3) +
      theme_bw() +
      scale_x_datetime() +
      facet_grid(Panel~., scales = "free_y")
  })

  output$history_plot <- renderPlot(get_history_plot() + theme(text = element_text(size = 24)))
  output$history_iplot <- renderPlotly(ggplotly(get_history_plot() + theme(text = element_text(size = 16))))
  # note: dynamic ticks does not work because of date axis
  default_history_plot_name <-
    reactive(paste0(Sys.time() %>% format("%Y%m%d_"), paste(input$history_variables, collapse = "_"), "_history.pdf"))
  callModule(plotDownloadDialog, "history_plot_download", get_history_plot, default_history_plot_name)

})
