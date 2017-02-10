# PARAMETER HISTORY ----
get_history_data <- reactivePoll(
  3000, session,
  # checks every second whether any of the history files were modified
  checkFunc = function() {
    sapply(HISTORY_FILES, function(path) {
      if(file.exists(path)) file.mtime(path) else ""
    }) %>% paste(collapse = "|")
  },
  # reads the log files
  valueFunc = function() {
    message("INFO: re-loading history files")
    files <- data_frame()
    tryCatch({
      files <-
        data_frame(
          filepath = HISTORY_FILES,
          Category = names(HISTORY_FILES)
        ) %>% group_by(filepath, Category) %>%
        do({
          if (file.exists(.$filepath[1])) {
            suppressWarnings(read.csv(.$filepath[1], header = TRUE, stringsAsFactors = FALSE,
                     row.names = NULL, colClasses = c(timestamp = "POSIXct", user = "character",
                                                      mode = "character", notes = "character"))) %>%
              gather(Column, value, -timestamp, -user, -mode, -notes)
          } else {
            data_frame()
          }
        }) %>%
        ungroup()
      if (nrow(files) > 0) {
        files <- files %>%
          mutate(Category = as.character(Category)) %>% # avoid factor + character join
          left_join(
            rename(parameters, mode_def = Mode),
            by = c("Category", "Column")) %>%
          mutate(date = as.Date(timestamp, tz = format(timestamp[1], "%Z"))) %>%
          select(-filepath) %>%
          # remove entries without matching defintion and missing values
          filter(!is.na(Type), !is.na(value), !is.na(mode_def)) %>%
          # make sure no incorrect replicates
          filter(is.na(mode) || mode_def == "all" || mapply(grepl, mode, mode_def, fixed=TRUE))
      }
    }, error = function(e) message("ERROR: problem loading history file - perhaps configuration changed, in which case saving a new history record will solve the problem, error: ", e$message),
    warning = function(w) message("WARNING: problem loading history files"))
    if (nrow(files) == 0) return(NULL)
    else return(files)
  }
)

# update the variable selection box and date range
observe({
  validate(need(get_history_data(), message = FALSE))

  if (is.null(input$history_mode) || is.null(input$history_category)) {
    options <- c("No parameters available")
    selected <- c()
  } else {
    history_vars <- get_history_data() %>%
      filter(Type == "numeric") %>%  # only numerics
      filter( # figure out if the selected modes are part of the mode_def
        mode_def == "all" |
        sapply(mode_def, function(m_def) {
          any(mapply(grepl, input$history_mode, m_def, fixed = TRUE))
        })) %>%
      filter(Category %in% input$history_category) %>%
      mutate(Label = paste0(Caption, " [", Units, "]: ", mode_def, "")) %>%
      select(Column, Label) %>%
      distinct() %>% arrange(Column)
    options <- history_vars$Column %>% setNames(history_vars$Label)
    selected <- options[options %in% isolate(values$history_variables)]
  }


  # update select input and date range input
  updateSelectInput(session, "history_variables", choices = options, selected = selected)

  time_range <- range(get_history_data()$date)
  updateDateRangeInput(session, "history_date_range",
                       min = time_range[1], max = time_range[2],
                       start = time_range[1], end = time_range[2])
})

# save the selected rows in the reactive values
observe({
  validate(need(input$history_variables, message = FALSE))
  selected_variables <- input$history_variables
  message("INFO: history parameter selection: '", paste(selected_variables, collapse = ", "), "'") # debug
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
    mutate(Date = timestamp, Value = value, Panel = paste(Caption, Units),
           Mode = ifelse(mode_def == "all" | is.na(mode), mode_def, mode)) %>%
    ggplot() + aes(x = Date, y = Value, color = Mode,
                   text = paste0(Panel, "<br>Active mode: ", mode, "<br>User: ", user, "<br>Notes: ", notes)) +
    geom_line(map = aes(group = paste(Panel, Mode)), linetype = 1) +
    geom_point(size = 3) +
    theme_bw() +
    scale_x_datetime() +
    facet_grid(Panel~., scales = "free_y")
})

output$history_plot <- renderPlot(get_history_plot() + theme(text = element_text(size = 24)))
output$history_iplot <- renderPlotly(ggplotly(get_history_plot() + theme(text = element_text(size = 16)),
                                              tooltip = c("text", "x", "y")))
# note: dynamic ticks does not work because of date axis
default_history_plot_name <-
  reactive(paste0(Sys.time() %>% format("%Y%m%d_"), paste(input$history_variables, collapse = "_"), "_history.pdf"))
callModule(plotDownloadDialog, "history_plot_download", get_history_plot, default_history_plot_name)
