#' CSIA UI
#'
#' Generates the user interface part of the isoviewer app
#'
#' @param app_title the title of the application
#' @param app_color the dashboard color, see \link[shinydashboard]{dashboardPage} skin for available options
app_ui <- function(app_title = "CSIA", app_color = "red", timezone = NULL) {

  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = app_color)

  dashboardPage(
    # SKIN ----
    skin = app_color,

    # HEADER ----
    dashboardHeader(title = app_title, titleWidth = 150),

    # SIDEBAR ---
    dashboardSidebar(

      width = 150,
      sidebarMenu(
        id = "menu",
        h5("isorunCSIA", as.character(packageVersion("isorunCSIA")), align = "center"),
        if (!is.null(timezone)) h5(timezone, align = "center"),

        menuItem("Welcome", tabName = "welcome", icon = icon("info")),
        menuItem("Instrument", tabName = "instrument", icon = icon("cog")),
        menuItem("Tuning", tabName = "tuning", icon = icon("music")),
        menuItem("Standards", tabName = "standards", icon = icon("check")),
        menuItem("Data", tabName = "data", icon = icon("pie-chart")),
        menuItem("Scans", tabName = "scans", icon = icon("bar-chart")),
        menuItem("Settings", tabName = "settings", icon = icon("wrench")),
        uiOutput("modes")
        #radioButtons("mode", label = "Mode", choices = modes$Mode),
      ),

      # HEADER ----
      tags$head(
        tags$style(
          type="text/css",
          HTML(str_c(
            # custom background box
            sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", box_default, box_default),
            sprintf(".box.box-solid.box-info{border:1px solid %s;}", box_default),
            sep="\n"))
        )
      ),

      # USE SHINY JS AND EXTENSIONS ---
      useShinyjs()

    ),

    # BODY ====
    dashboardBody(

      # div(class = "row",
      #     tabItems(
      #       # login ====
      #       tabItem("login", loginUI("login", title = app_title)),
      #
      #       # all other tabs ====
      #       tabItem("data", div(id = "data-panel", column(width = 12, uiOutput("data") %>% withSpinner(type = 5, proxy.height = "450px")))),
      #       tabItem("devices", div(id = "devices-panel", column(width = 12, uiOutput("devices")))),
      #       tabItem("experiments", div(id = "experiments-panel", column(width = 12, uiOutput("experiments")))),
      #       tabItem("live", div(id = "live-panel", column(width = 12, uiOutput("live"))))
      #       ## old live
      #       # h2(
      #       #   actionLink("refresh_cams", "Reload cameras", icon = icon("gear")),
      #       #   bsTooltip("refresh_cams", "Reload the cameras"),
      #       #   align = "center"
      #       # ),
      #       # uiOutput("raspicams") %>% withSpinner(type = 5, proxy.height = "480px")
      #
      #     )
      # )
    )
  )

}
