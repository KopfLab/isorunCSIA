library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyAce)
library(magrittr)
library(isorunCSIA)
library(ggplot2)
library(plotly)
library(readxl)
library(rhandsontable)
library(dtplyr)
library(dplyr)
library(tidyr)
library(DT)
`%then%` <- shiny:::`%OR%`

# modules
source("module_file_browser.R")
source("module_ggplot_download.R")
source("module_history_info_entry.R")
source("module_data_table.R")
source("utils.R")

# make sure base directory is set
if (!exists(".base_dir", env = .GlobalEnv))
  .GlobalEnv$.base_dir <- file.path(getwd(), "data")

# fixed settings
SIDEBAR_WIDTH <- 150 #px
SETTINGS_FILE <- "isorunCSIA_settings.xlsx"
INSTRUMENT_HISTORY_FOLDER <- "instrument_history"
HISTORY_FILES <- c(
  "background" = file.path(.base_dir, INSTRUMENT_HISTORY_FOLDER, "instrument_background_history.csv"),
  "sensitivity" = file.path(.base_dir, INSTRUMENT_HISTORY_FOLDER, "instrument_sensitivity_history.csv"),
  "parameters" = file.path(.base_dir, INSTRUMENT_HISTORY_FOLDER, "instrument_parameters_history.csv")
)
FULL_SCAN_FOLDER <- file.path(INSTRUMENT_HISTORY_FOLDER, "full_scans")
PEAK_SHAPE_FOLDER <- file.path(INSTRUMENT_HISTORY_FOLDER, "peak_shapes")

# make sure folders exist
if (!file.exists(.base_dir))
  dir.create(.base_dir)
if (!file.exists(file.path(.base_dir, INSTRUMENT_HISTORY_FOLDER)))
  dir.create(file.path(.base_dir, INSTRUMENT_HISTORY_FOLDER))
if (!file.exists(file.path(.base_dir, FULL_SCAN_FOLDER)))
  dir.create(file.path(.base_dir, FULL_SCAN_FOLDER))
if (!file.exists(file.path(.base_dir, PEAK_SHAPE_FOLDER)))
  dir.create(file.path(.base_dir, PEAK_SHAPE_FOLDER))

# copy default settings if needed
.settings_file <- file.path(.GlobalEnv$.base_dir, SETTINGS_FILE)
if (!file.exists(.settings_file)) {
  message("INFO: No settings file exists in this workspace yet. Copying default settings file.")
  .default_settings_file <- system.file("gui", "default_settings.xlsx", package = "isorunCSIA")
  file.copy(.default_settings_file, .settings_file)
}


