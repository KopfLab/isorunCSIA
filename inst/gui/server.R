

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
    full_scan_file = NULL, # last saved full scan file
    peak_shape_file = NULL, # last saved peak shape file
    history_variables = c()
  )

  # INSTRUMENT NEW PARAMETER RECORD ----
  observe({ input$instrument_new_clear; values$full_scan_file <- NULL; values$peak_shape_file <- NULL }) # reset
  source("server_background.R", local = TRUE)
  source("server_sensitivity.R", local = TRUE)
  source("server_instrument_parameters.R", local = TRUE)

  # PARAMETER HISTORY
  #source("server_parameter_history.R", local = TRUE)


})
