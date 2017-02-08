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
                                       mutate(Category = as.character(Category)) %>% # avoid factor - character join
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
