### DATA

scan_files_select <- callModule(fileSelector, "scan_files_select", pattern = "\\.scn$",
                              root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                              number_recent = 50, sort_desc = TRUE,
                              exclude_recent = INSTRUMENT_HISTORY_FOLDER)


# add files to the scanfiles list
observe({
  scan_files_select$modal_closed()
  isolate({
    files <- scan_files_select$selection_relative()
    if (length(files) > 0) {
      files <- files[grepl("\\.scn$",files)]
      files <- files[!files %in% values$scan_files_list]
      message("INFO: adding ", length(files), " new scan files to list")
      values$scan_files_list <- c(values$scan_files_list, files) %>% unique() %>% sort()
      selected <- values$scan_files_list[values$scan_files_list %in% values$scan_files_selected]
      updateSelectInput(session, "scan_files_list",
                        choices = values$scan_files_list, selected = selected)
    }
  })
})

# keep track of what is selected in the scan files list
observe({
  validate(need(input$scan_files_list, message = FALSE))
  message("INFO: ", length(input$scan_files_list), " scan files selected")
  isolate(values$scan_files_selected <- input$scan_files_list)
})

# trigger scans load
observe({
  req(!is.null(input$scan_files_load) |
        !is.null(input$scan_file_list_dblclick))
  isolate({
     message("INFO: Loading ", length(input$scan_files_list), " scan files' data")
     values$scan_files_data <- get_scan_files_data()
  })
})

# scan files download save
output$scan_files_download <- downloadHandler(
  filename = function() { paste0(Sys.time() %>% format("%Y%m%d"), "_scans.zip") },
  content = function(file) {
    files <- file.path(data_dir, isolate(values$scan_files_selected))
    zip(file, files = files, extra = "-j")
  },
  contentType = "application/zip"
)

# generatte scan plots
output$scans_plot <- renderPlot({
  generate_scans_plot() + theme(text = element_text(size = 24))
})
output$scans_iplot <- renderPlotly({
  ggplotly(generate_scans_plot() +
             theme(legend.position = "none") +
             theme(text = element_text(size = 16)))
})

# download scans plot
default_scans_plot_name <-
  reactive(paste0(Sys.time() %>% format("%Y%m%d_"), "_scan_plot.pdf"))
callModule(plotDownloadDialog, "scans_plot_download",
           generate_scans_plot,
           default_scans_plot_name)

#--- PLOTTING functions

# generate the scans plot
generate_scans_plot <- reactive({
  req(values$scan_files_data)
  message("INFO: Generating scans plot")
  withProgress(
    message = 'Generating plot...', value = 0, {
      incProgress(0.25, detail = "Assembling.")
      plot <-
        values$scan_files_data %>%
        mutate(
          Mass = mass,
          File = file
        ) %>%
        ggplot() +
        aes(step, intensity, color =  Mass) +
        geom_line() +
        theme_bw() +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "Steps", y = "Intensity") +
        facet_wrap(~File, scales = "free", ncol = 1)
      incProgress(0.5, detail = "Rendering.")
    })
  return(plot)
})

#--- UTILITY functions

# retrieve scan traces (only when called explicitly hence not reactive)
get_scan_files_data <- function() {
  # make sure selected scan files are loaded
  values$scan_files_objects <- load_iso_data(input$scan_files_list, loaded = values$scan_files_objects, root = data_dir)
  get_scan_mass_traces(values$scan_files_objects[names(values$scan_files_objects) %in% input$scan_files_list])
}

#' get scan trace data (could be an exported function?)
get_scan_mass_traces <- function(files) {
  data <- files %>% lapply(function(i) mutate(i$get_mass_data(melt = T), file = i$filename))
  data <- suppressWarnings(data %>% bind_rows() %>% as_data_frame())
  if (nrow(data) == 0) return(NULL)
  else return(data)
}
