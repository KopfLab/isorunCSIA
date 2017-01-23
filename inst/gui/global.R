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
`%then%` <- shiny:::`%OR%`

# modules
source("module_file_browser.R")
source("module_ggplot_download.R")
source("module_history_info_entry.R")
source("utils.R")

# fixed settings
SIDEBAR_WIDTH <- 150 #px
SETTINGS_FILE <- "isorunCSIA_settings.xlsx"
INSTRUMENT_HISTORY_FOLDER <- "instrument_history"
HISTORY_FILES <- c(
  "background" = "instrument_background_history.csv",
  "sensitivity" = "instrument_sensitivity_history.csv",
  "parameters" = "instrument_parameters_history.csv"
)
ELEMENTS <- c("carbon", "hydrogen", "nitrogen")

# make sure base directory is set
if (!exists(".base_dir", env = .GlobalEnv))
  .GlobalEnv$.base_dir <- file.path(getwd(), "data")

# copy default settings if needed
.settings_file <- file.path(.GlobalEnv$.base_dir, SETTINGS_FILE)

if (!file.exists(.settings_file)) {
  message("INFO: No settings file exists in this workspace yet. Copying default settings file.")
  .default_settings_file <- system.file("gui", "default_settings.xlsx", package = "isorunCSIA")
  file.copy(.default_settings_file, .settings_file)
}


