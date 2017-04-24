# SERVER =====
server <- shinyServer(function(input, output, session) {

  # SETTINGS ----
  message("\n\nINFO: Loading GUI instance ...")
  message("INFO: Loading settings ...")
  global <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "global")
  modes <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "modes")
  parameters <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "parameters")

  # REACTIVE VALUES ----
  values <- reactiveValues(
    full_scan_file = NULL, # last saved full scan file
    peak_shape_file = NULL, # last saved peak shape file
    tuning_peak_shape_file = NULL, # last saved tuning peak shape file
    history_variables = c(),
    data_files_list = c(),
    data_files_selected = c(),
    data_files_objects = list(),
    data_files_table_data = NULL,
    data_files_mass_data = NULL,
    scan_files_list = c(),
    scan_files_selected = c(),
    scan_files_objects = list(),
    scan_files_data = NULL
  )

  # INSTRUMENT NEW PARAMETER RECORD ----
  observe({ # reset
    input$instrument_new_clear
    values$full_scan_file <- NULL
    values$peak_shape_file <- NULL
    values$on_off_data_table <- NULL
    updateTextInput(session, "user", value = "")
  })
  source("server_background.R", local = TRUE)
  source("server_sensitivity.R", local = TRUE)
  source("server_instrument_parameters.R", local = TRUE)
  source("server_linearity_on_offs.R", local = TRUE)

  # TUNING
  observe({ # reset
    input$tuning_new_clear
    values$tuning_peak_shape_file <- NULL
    updateTextInput(session, "tuning_user", value = "")
  })
  source("server_tuning.R", local = TRUE)

  # PARAMETER HISTORY
  source("server_parameter_history.R", local = TRUE)

  # DATA
  source("server_data_viewer.R", local = TRUE)

  # SCANS
  source("server_scans_viewer.R", local = TRUE)


})
