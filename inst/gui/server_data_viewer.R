### DATA

data_files_select <- callModule(fileSelector, "data_files_select", pattern = "\\.dxf$",
                              root = data_dir, root_name = "All", size = 12, multiple = TRUE,
                              number_recent = 30, exclude_recent = INSTRUMENT_HISTORY_FOLDER)


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
      selected <- files[values$data_files_list %in% values$data_files_selected]
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

# plot selected
output$data_files_data_table <- DT::renderDataTable({

  validate(need(input$data_files_plot, message = FALSE))
  isolate({
    message("INFO: Activating ", length(input$data_files_list), " data files")
    data <- get_data_files_table_data()
  })

  datatable(data,
            rownames = FALSE, selection = "none",
            #extensions = c("FixedHeader", "Scroller"), # for scrolling instead of pagination
            extensions = c("Buttons", "ColReorder", "KeyTable"),
            options = list(
              dom = 'Bftpli', # Buttons, Filter, Table, Pagniation, Length, Information
              pageLength = 5, lengthMenu = c(5, 10, 15, 20, 50, 100), # paging menu
              keys = TRUE, #KeyTable extension
              #fixedHeader = TRUE, #FixedHeader extension
              #deferRender = TRUE, scrollY = 200, scroller = TRUE, # Scroller extension
              scrollX = TRUE,
              colReorder = TRUE, # ColReorder extension, allow columns reorder
              buttons = list(list(
                extend = "colvis", columns = 0:(ncol(data)-1) # Buttons extension, allow column selection
              ))
            )
  ) %>% formatSignif(sapply(data, class) == "numeric", digits = 3)
}, server = FALSE)

observe({

})

# retrieve mass traces (only when called explicitly hence not reactive)
get_data_files_mass_data <- function() {
  # make sure selected data files are loaded
  values$data_files_objects <- load_iso_data(input$data_files_list, loaded = values$data_files_objects, root = data_dir)
  get_iso_mass_traces(values$data_files_objects[input$data_files_list])
}

# retrieve data tables (only when called explicitly hence not reactive)
get_data_files_table_data <- function() {
  # make sure selected data files are loaded
  values$data_files_objects <- load_iso_data(input$data_files_list, loaded = values$data_files_objects, root = data_dir)
  get_iso_data_tables(values$data_files_objects[input$data_files_list])
}

#--- UTILITY functions

#' get mass trace data (could be an exported function?)
get_iso_mass_traces <- function(files) {
  data <- lapply(files, function(i) mutate(i$get_mass_data(melt = T)[c("time", "signal", "variable")], file = i$filename))
  suppressWarnings(as_data_frame(bind_rows(data)))
}

#' get table data (could be an export function?)
get_iso_data_tables <- function(files){
  files %>%
    lapply(function(file) {
      dt <- file$get_data_table() %>% arrange(row_number())
      rows_set1 <- dt %>% filter(!is.na(`Nr.`))
      rows_set2 <- dt %>% filter(is.na(`Nr.`))
      cols1 <- names(rows_set1)[sapply(rows_set1, function(col) !all(is.na(col)))]
      cols2 <- names(rows_set2)[sapply(rows_set2, function(col) !all(is.na(col)))]
      cols2 <- cols2[!cols2 %in% cols1] # avoid duplicates

      mutate(cbind(rows_set1[cols1], rows_set2[cols2]), File = file$filename)[c("File", names(dt))]
    }) %>% bind_rows() %>% as_data_frame()
}

#' load isotope data (could be an exported function)
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
          iso_file <- list(isoread::isoread(file.path(root, file), type = "CFLOW", quiet = quiet)) %>% setNames(file)
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
