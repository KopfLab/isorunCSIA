

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

  # REACTIVE VALUES ----
  values <- reactiveValues(
    tuning_file_preview = NULL,
    history_variables = c()
  )

  # INSTRUMENT NEW PARAMETER RECORD ----
  history_files <- expand.grid(Element = ELEMENTS, Category = names(HISTORY_FILES)) %>%
    mutate(
      filename = HISTORY_FILES[Category],
      filepath = file.path(data_dir, INSTRUMENT_HISTORY_FOLDER, paste0(Element, "_", filename))
    )
  background_table <- callModule(
    historyInfoTable, "background", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear))

  sensitivity_table <- callModule(
    historyInfoTable, "sensitivity", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear))

  instrument_table <- callModule(
    historyInfoTable, "parameters", parameters = parameters, history_files = history_files,
    element_input = reactive(input$element), clear_input = reactive(input$instrument_new_clear))

  # INSTRUMENT PARAMETER HISTORY ----
  get_history_data <- reactivePoll(100000, session,
    # checks every second whether any of the history files were modified
    checkFunc = function() {
      sapply(history_files$filepath, function(path) {
        if(file.exists(path)) file.info(path)$mtime[1] else ""
      }) %>% paste(collapse = "|")
    },
    # reads the log files
    valueFunc = function() {
      message("INFO: re-loading history files")
      history_files %>% group_by(Element, Category, filepath) %>%
        do({
          print(.$filepath[1])
          if (file.exists(.$filepath[1])) {
            read.csv(.$filepath[1], header = TRUE, stringsAsFactors = FALSE,
                     row.names = NULL, colClasses = c(timestamp = "POSIXct")) %>%
              gather(Column, value, -timestamp, -Notes)
          } else {
            data_frame()
          }
        }) %>%
        ungroup() %>%
        left_join(
          rename(parameters, Element_def = Element),
          by = c("Category", "Column"))
    }
  )

  # update the selection box
  observe({
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
  })

  # save the selected rows in the reactive values
  observe({
    selected_variables <- input$history_variables
    message("INFO: storing selection") # debug
    isolate(values$history_variables <- selected_variables)
  })


  # TUNING: File selection ----
  tuning_files <- callModule(fileSelector, "tuning_files_local", pattern = "\\.(scn|bff)$",
                             root = data_dir, root_name = "Available", size = 8)
  data_files <- callModule(fileSelector, "data_files_local", pattern = "\\.did$",
                           root = data_dir, root_name = "Available", size = 8)




  # TUNING ----




  # TUNING: File quick view plot ----
  get_tuning_file_plot <- reactive({
    view_file <- tuning_files$double_click()
    validate(
      need(view_file, message = "No file selected (double click scan file to load).") %then%
        need(!dir.exists(view_file), message = FALSE) %then%
        need(file.exists(view_file), message = "File does not exists") %then%
        need(grepl("\\.(scn|bff)$", view_file),
             message = sprintf("Cannot preview file %s. Supported file types are '.scn' and '.bff'.",
                               basename(view_file)))
    )

    scans <- load_scans(view_file)
    message("Generating tuning file plot for ", view_file)
    make_ggplot(scans[[1]])
  })

  # render and download
  output$tuning_file_plot <- renderPlot(get_tuning_file_plot())
  output$tuning_file_iplot <- renderPlotly({
    get_tuning_file_plot()
    scans <- load_scans(tuning_files$double_click(), quiet = T)
    message("Converting to interactive plot")
    make_iplot(scans[[1]])
  })
  default_tuning_filename <- reactive(sub("\\.[^.]+", ".pdf", basename(tuning_files$double_click())))
  callModule(plotDownloadDialog, "tuning_file_download", get_tuning_file_plot, default_tuning_filename)

  # TUNING: File plot code preview ----
  observe({
    view_file <- tuning_files$double_click()
    if (is.null(view_file) || view_file == "") code <- "```{r}\n# No file selected\n```"
    else {
      code <-
        paste0(
          "```{r}\n",
          "# Load library\nlibrary(dpos)\n\n",
          "# Code for plot generation\n",
          "scans <- load_scans(file.path(\"path\", \"to\", \"%s\"))\n",
          "scan <- scans[[1]]\n",
          "make_%splot(scan)\n",
          "```") %>%
        sprintf(basename(view_file), input$tuning_file_plot_tabs)
    }

    updateAceEditor(session, "tuning_plot_code", value = code)
  })


  # DATA ----

  # DATA: File selection ----


  # DATA: File quick view plot ----
  get_data_file_plot <- reactive({
    view_file <- data_files$double_click()
    validate(
      need(view_file, message = "No file selected (double click data file to load).") %then%
        need(!dir.exists(view_file), message = FALSE) %then%
        need(file.exists(view_file), message = "File does not exists") %then%
        need(grepl("\\.did$", view_file),
             message = sprintf("Cannot preview file %s. Supported file types are '.did'.",
                               basename(view_file)))
    )

    data <- load_isodat_data(view_file)
    message("Generating data file plot for ", view_file)
    make_ggplot(data)
  })

  # render and download
  output$data_file_plot <- renderPlot(get_data_file_plot())
  output$data_file_iplot <- renderPlotly({
    get_data_file_plot()
    data <- load_isodat_data(data_files$double_click(), quiet = T)
    message("Converting to interactive plot")
    make_iplot(data)
  })
  default_data_filename <- reactive(sub("\\.[^.]+", ".pdf", basename(data_files$double_click())))
  callModule(plotDownloadDialog, "data_file_download", get_data_file_plot, default_data_filename)

  # DATA: File plot code preview ----
  observe({
    view_file <- data_files$double_click()
    if (is.null(view_file) || view_file == "") code <- "# No file selected"
    else {
      code <-
        paste0(
          "# Load library\nlibrary(dpos)\n\n",
          "# Code for plot generation\n",
          "data <- load_isodat_data(file.path(\"path\", \"to\", \"%s\"))\n",
          "make_%splot(data)") %>%
        sprintf(basename(view_file), input$data_file_plot_tabs)
    }

    updateAceEditor(session, "data_plot_code", value = code)
  })


})
