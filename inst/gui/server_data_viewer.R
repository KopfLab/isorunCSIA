### DATA

data_files_select <- callModule(fileSelector, "data_files_select", pattern = "\\.dxf$",
                              root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                              number_recent = 50, sort_desc = TRUE,
                              exclude_recent = INSTRUMENT_HISTORY_FOLDER)


# add files to the data files list
observe({
  data_files_select$modal_closed()
  isolate({
    files <- data_files_select$selection_relative()
    if (length(files) > 0) {
      files <- files[grepl("\\.dxf$",files)]
      files <- files[!files %in% values$data_files_list]
      message("INFO: adding ", length(files), " new data files to list")
      values$data_files_list <- c(values$data_files_list, files) %>% unique() %>% sort()
      selected <- values$data_files_list[values$data_files_list %in% values$data_files_selected]
      updateSelectInput(session, "data_files_list",
                        choices = values$data_files_list, selected = selected)
    }
  })
})

# keep track of what is selected in the data files list
observe({
  validate(need(input$data_files_list, message = FALSE))
  message("INFO: ", length(input$data_files_list), " data files selected")
  isolate(values$data_files_selected <- input$data_files_list)
})

# keep track of double click in the data files list
observe({
  message("here: ", input$data_file_list_dblclick)
})

# remove data files from consideration
observe({
  validate(need(input$data_files_remove, message = FALSE))
  isolate({
    # update list
    if  (!is.null(input$data_files_list) && length(input$data_files_list) > 0) {
      message("INFO: removing ", length(input$data_files_list), " from list")
      values$data_files_list <- values$data_files_list[!values$data_files_list %in% input$data_files_list]
      updateSelectInput(session, "data_files_list", choices = values$data_files_list, selected = c())
    }
  })
})

# data files table
data_files_table <- callModule(serverDataTable, "data_files_table", selection = "none") # no selection for now
observe({ data_files_table$rows_selected() }) # no selection allowed for now

# trigger mass data load
observe({
  req(!is.null(input$data_files_load) |
        !is.null(input$data_file_list_dblclick))
  isolate({
     message("INFO: Loading ", length(input$data_files_list), " data files' mass data")
     values$data_files_mass_data <- get_data_files_mass_data()
  })
})

# trigger table data load
observe({
  req(!is.null(input$data_files_load) |
        !is.null(input$data_files_export) |
        !is.null(input$data_file_list_dblclick))
  isolate({
    message("INFO: Loading ", length(input$data_files_list), " data files' table data")
    values$data_files_table_data <- get_data_files_table_data()
  })
})

# data export default filename
observe({
  req(input$data_files_export)
  updateTextInput(session, "data_files_export_name",
                  value = paste0(Sys.time() %>% format("%Y%m%d_"), "_data_export.xlsx"))
})

# data export button enabled/disabled
observe({
  if (is.null(input$data_files_export_columns) || length(input$data_files_export_columns) == 0)
    disable("data_files_export_save")
  else
    enable("data_files_export_save")
})

# data export save
output$data_files_export_save <- downloadHandler(
  filename = function() { isolate(input$data_files_export_name) },
  content = function(file) {
    isolate({
      message("INFO: Exporting data to ", input$data_files_export_name)
      values$data_files_table_data[input$data_files_export_columns] %>%
        openxlsx::write.xlsx(file = file)
    })
  })

# data files download save
output$data_files_download <- downloadHandler(
  filename = function() { paste0(Sys.time() %>% format("%Y%m%d"), "_data.zip") },
  content = function(file) {
    files <- file.path(data_dir, isolate(values$data_files_selected))
    zip(file, files = files, extra = "-j")
  },
  contentType = "application/zip"
)

# data table
observe({
  req(values$data_files_table_data)
  data_files_table$update(values$data_files_table_data)
  updateSelectInput(session, "data_files_export_columns",
                    choices = names(values$data_files_table_data),
                    selected = c()) # fixme: should keep selection if possible
})

# generatte data plots
output$data_plot <- renderPlot({
  generate_data_plot() + theme(text = element_text(size = 24))
})
output$data_iplot <- renderPlotly({
  ggplotly(generate_data_plot() +
             theme(legend.position = "none") +
             theme(text = element_text(size = 16)))
})

# download data plot
default_data_plot_name <-
  reactive(paste0(Sys.time() %>% format("%Y%m%d_"), "_data_plot.pdf"))
callModule(plotDownloadDialog, "data_plot_download",
           generate_data_plot,
           default_data_plot_name)

#--- PLOTTING functions

# generate the data overview plot (chromatogram)
generate_data_plot <- reactive({
  req(values$data_files_mass_data)
  message("INFO: Generating data plot")
  withProgress(
    message = 'Generating plot...', value = 0, {
      incProgress(0.25, detail = "Assembling.")
      plot <-
        values$data_files_mass_data %>%
        mutate(
          Mass = variable,
          File = file
        ) %>%
        ggplot() +
        aes(time, signal, linetype = Mass, colour = File) +
        geom_line() +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "Time [s]", y = "Signal [mV]", linetype = "Trace", colour = "File") +
        theme_bw()
      incProgress(0.5, detail = "Rendering.")
    })
  return(plot)
})

#--- UTILITY functions

# retrieve mass traces (only when called explicitly hence not reactive)
get_data_files_mass_data <- function() {
  # make sure selected data files are loaded
  values$data_files_objects <- load_iso_data(input$data_files_list, loaded = values$data_files_objects, root = data_dir)
  get_iso_mass_traces(values$data_files_objects[names(values$data_files_objects) %in% input$data_files_list])
}

# retrieve data tables (only when called explicitly hence not reactive)
get_data_files_table_data <- function() {
  # make sure selected data files are loaded
  values$data_files_objects <- load_iso_data(input$data_files_list, loaded = values$data_files_objects, root = data_dir)
  get_iso_data_tables(values$data_files_objects[names(values$data_files_objects) %in% input$data_files_list])
}

#' get mass trace data (could be an exported function?)
get_iso_mass_traces <- function(files) {
  data <-
    files %>%
    lapply(function(i) mutate(i$get_mass_data(melt = T)[c("time", "signal", "variable")], file = i$filename)) %>%
    bind_rows() %>%
    as_data_frame()
  if (nrow(data) == 0) return(NULL)
  else return(data)
}

#' get table data (could be an export function?)
get_iso_data_tables <- function(files){
  data <- files %>%
    lapply(function(file) {
      dt <- file$get_data_table() %>% arrange(row_number())
      rows_set1 <- dt %>% filter(!is.na(`Nr.`))
      rows_set2 <- dt %>% filter(is.na(`Nr.`))
      cols1 <- names(rows_set1)[sapply(rows_set1, function(col) !all(is.na(col)))]
      cols2 <- names(rows_set2)[sapply(rows_set2, function(col) !all(is.na(col)))]
      cols2 <- cols2[!cols2 %in% cols1] # avoid duplicates
      mutate(cbind(rows_set1[cols1], rows_set2[cols2]), File = file$filename)[c("File", names(dt))]
    }) %>%
    bind_rows() %>%
    as_data_frame()
  if (nrow(data) == 0) return(NULL)
  else {
    # sanitize column names (remove trailing white spaces)
    names(data) <- trimws(names(data))
    return(data)
  }
}

#' load isotope data (could be an exported function except for progress)
load_iso_data <- function(files, loaded = c(), root = ".", quiet = FALSE) {

  # check which files have not been loaded yet
  not_loaded_yet <- setdiff(files, names(loaded))

  if ( (n <- length(not_loaded_yet)) > 0) {
    message("INFO: loading ", n, " data files")

    # read isodat files
    iso_files <- list()
    withProgress(message = 'Loading data...', value = 0, {
      for (file in not_loaded_yet) {
        incProgress(1/n, detail = paste0("Reading ", file, " ..."))
        tryCatch({
          iso_file <- list(isoread::isoread(file.path(root, file), quiet = quiet)) %>% setNames(file)
          iso_files <- c(iso_files, iso_file)
        },
        error = function(e) message("ERROR: encountered error while reading file ", file, ": ", e$message),
        warning = function(w) message("WARNING: encountered warning while reading file ", file, ": ", w$message))
      }
    })

    return(c(loaded, iso_files))
  } else {
    return(loaded)
  }
}
