#' Run the user interface
#'
#' This function runs the user interface.
#'
#' @param instrument_id which instrument to run for
#' @param pool ideally database connection pool, see \link[pool]{dbPool} but can also be a single db connection (not recommended)
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' (note: if \code{launch=FALSE}, \code{...} gets ignored)
#' @inheritParams app_ui
#' @export
csia_run_gui <- function(instrument_id, pool, timezone = Sys.timezone(), app_title = instrument_id, app_color = "red", ..., launch = FALSE) {

  # start-up message
  glue::glue("\n***************************************************************",
          "\nINFO: Launching isorunCSIA GUI (version {as.character(packageVersion('isorunCSIA'))}) ",
          "for instrument '{instrument_id}' in timezone {timezone}...",
          if (default("debug")) "\nINFO: Debug mode is turned ON" else ""
  ) %>% message()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = app_ui(app_title = app_title, app_color = app_color, timezone = timezone),
    server = app_server(instrument_id = instrument_id, pool = pool, timezone = timezone)
  )

  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}
