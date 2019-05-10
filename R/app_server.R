#' GUI Server
#'
#' Generates the server part of the app
app_server <- function(instrument_id, pool, timezone, start_screen = "data") {
  shinyServer(function(input, output, session) {

    message("\n\nINFO: Loading GUI instance ...")

    # # data managers
    # dm_experiments <- callModule(experimentsDataServer, "dm_experiments", group_id, access_token, pool, timezone)
    # dm_devices <- callModule(devicesDataServer, "dm_devices", group_id, access_token, pool, timezone)
    # dm_datalogs <- callModule(datalogsDataServer, "dm_datalogs", dm_experiments, dm_devices, group_id, access_token, pool, timezone)
    # dm_cloudinfo <- callModule(cloudInfoDataServer, "dm_cloudinfo", dm_experiments, dm_devices, group_id, access_token, pool, timezone)
    #
    # # login server
    # login_manager <- callModule(loginServer, "login", app_pwd = app_pwd, group = group_id, timezone = timezone)
    # observeEvent(input$menu, {
    #   if (!login_manager$is_logged_in()) {
    #       module_message(NULL, "debug", "not logged in yet, jumping back to login screen")
    #       updateTabItems(session, "menu", selected = "login")
    #   }
    # })
    # observeEvent(login_manager$is_logged_in(), {
    #   if (login_manager$is_logged_in()) {
    #     updateTabItems(session, "menu", start_screen)
    #     dm_experiments$refresh_experiments(init = TRUE)
    #     dm_devices$refresh_devices(init = TRUE)
    #   }
    # })
    #
    # # DATA SCREEN ====
    # callModule(experimentSelectorServer, "data_exps", dm_experiments)
    # data_plot <- callModule(
    #   dataPlotServer, "data_plot", timezone = timezone,
    #   get_experiments = dm_experiments$get_selected_experiments,
    #   get_data_logs = dm_datalogs$get_experiments_data_logs,
    #   refresh_data_logs = dm_datalogs$refresh_data_logs,
    #   reset_plot = eventReactive(length(dm_experiments$get_selected_experiments()), runif(1))
    # )
    # output$data <- renderUI({
    #   if (!login_manager$is_logged_in()) return(NULL)
    #   isolate({
    #     message("INFO: Generating 'data' screen")
    #     tagList(
    #       experimentSelectorUI("data_exps"),
    #       dataPlotUI("data_plot")
    #     )
    #   })
    # })


  })
}
