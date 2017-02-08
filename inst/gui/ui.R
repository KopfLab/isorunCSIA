# settings
modes <- read_excel(file.path(.GlobalEnv$.base_dir, SETTINGS_FILE), sheet = "modes")

# Define UI that plots the isotope label enrichment
ui <- dashboardPage(

  # SKIN ----
  skin = "blue",

  # HEADER ----
  dashboardHeader(title = "Isorun CSIA"),

  # SIDEBAR ----
  sidebarMenu(

    menuItem("Welcome", tabName = "welcome", icon = icon("info")),
    menuItem("Instrument", tabName = "instrument", icon = icon("cog"), selected = TRUE),
    menuItem("Tuning", tabName = "tuning", icon = icon("music")),
    menuItem("Standards", tabName = "standards", icon = icon("check")),
    menuItem("Data", tabName = "data", icon = icon("bar-chart")),
    menuItem("Settings", tabName = "settings", icon = icon("wrench")),
    radioButtons("mode", label = "Mode", choices = modes$Mode),
    menuItem(
      "Tuning", tabName = "tunings", icon = icon("music"),
      menuSubItem("Select files", tabName = "tuning_files", icon = icon("files-o")),
      menuSubItem("Analysis", tabName = "tuning_analyze", icon = icon("bar-chart"))),

    # STYLESHEET ----
    tags$head(
      tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 16px; }")),
      tags$style(HTML(".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }")),
      tags$style(type = "text/css", ".sidebar {height:1300px}") # FIXME: make this dynamically long enough
    ),


    # USE SHINY JS ---
    shinyjs::useShinyjs()

  ) %>% dashboardSidebar(width = SIDEBAR_WIDTH),

  # BODY ----
  tabItems(

    # WELCOME ----
    tabItem(tabName = "welcome",
            h1("Welcome to the Isorun CSIA Toolbox")),

    # INSTRUMENT ----
    tabItem(
      tabName = "instrument",
      tabsetPanel(
        id = "instrument_tabs", selected = "new",
        tabPanel(
          "New", value = "new",

          p(h4(actionLink("instrument_new_clear", "Clear all", icon = icon("rotate-left")))),

          box(title = "Background", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning", # FIXME: change color with save status!
              historyInfoInput(id = "background"),
              historyArchiveButton(id = "background"),
              modalFileSelectorInput(id = "full_scan_files", open_label = "Save full scan file", link_wrapper = h4, allow_upload = FALSE),
              h5(textOutput("full_scan_file"))
          ),

          box(title = "Sensitivity & Peak shape", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning",
              historyInfoInput(id = "sensitivity"),
              historyArchiveButton(id = "sensitivity"),
              modalFileSelectorInput(id = "peak_shape_files", open_label = "Save peak shape file", link_wrapper = h4, allow_upload = FALSE),
              h5(textOutput("peak_shape_file"))
          ),

          box(title = "Instrument parameters", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning",
              historyInfoInput(id = "parameters"),
              historyArchiveButton(id = "parameters")
          ),

          box(title = "Isotopic stability", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning",
            h4("ON/OFFs"),
            h4("Linearity")
          )

        ),

        # PARAMETER HISTORY ----
        tabPanel(
          "Parameter History", value = "params",
          # Parameter selection box
          box(
            title = "Parameter selection", collapsible = TRUE,
            status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 6,
                     selectInput("history_mode", label = NULL, multiple = TRUE,
                                 choices = modes$Mode, selected = modes$Mode),
                     bsTooltip("history_mode", "Which modes to consider", placement = "top", trigger = "hover")
              ),
              column(width = 6,
                     selectInput("history_category", label = NULL, multiple = TRUE,
                                 choices = names(HISTORY_FILES), selected = names(HISTORY_FILES)),
                     bsTooltip("history_category", "Which parameter groups", placement = "top", trigger = "hover")
              )
            ),
            selectInput("history_variables", label = NULL, multiple = TRUE, size = 5, selectize = FALSE,
                        choices = c(), selected = c()),
            dateRangeInput("history_date_range", label = NULL,
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", separator = " to ")
          ),

          # Plots box
          box(
            title = "Parameter history",
            status = "success", solidHeader = TRUE, width = 12,
            plotDownloadLink(id = "history_plot_download"),
            tabsetPanel(
              id = "history_plot_tabs", selected = "i",
              tabPanel("Interactive Plot", value = "i",
                       plotlyOutput("history_iplot", height="500px", width = "100%")),
              tabPanel("Static Plot", value = "gg",
                       plotOutput("history_plot", height="500px", width = "100%"))
            )
          )
        ),


        tabPanel("Full scan History", value = "full_scans",
                 h1("temp")),
        tabPanel("Peak shape History", value = "peak_shapes",
                 h1("temp"))
      )
    ),

    # STANDARDS ----

    # DATA ----

    # SETTINGS ----

    # TUNING: File selection ----
    tabItem(
      tabName = "tuning_files",


      # TUNING: File preview box ----
      box(
        plotDownloadLink(id = "tuning_file_download"),

        tabsetPanel(id = "tuning_file_plot_tabs",
          tabPanel("Static Plot", value = "gg",
                   plotOutput("tuning_file_plot", height="500px", width = "100%")),
          tabPanel("Interactive Plot", value = "i",
                   plotlyOutput("tuning_file_iplot", height="500px", width = "100%"))
        ),

        title = "Tuning file quick view",
        status = "info", solidHeader = TRUE, width = 12),

      # TUNING: File preview code ----
      box(
        aceEditor("tuning_plot_code", mode = "r",
                  theme="ambiance", readOnly = TRUE,
                  height = "200px"),
        title = "Code preview",
        status = "success", solidHeader = TRUE, width = 12)
    ), # / tabItem

    # TUNING: Analysis ----
    tabItem(
      tabName = "tuning_analyze", h2("hello")),


    # DATA ----

    tabItem(
      tabName = "data",
      column(
        width = 12,
        fileSelectorInput(
          id = "data_files_local", allow_upload = TRUE,
          upload_label = 'Upload tuning files (individual or .zip archives)')) %>%
        fluidRow(),

      # DATA: File preview code ----
      box(
        aceEditor("data_plot_code", mode = "r",
                  theme="ambiance", readOnly = TRUE,
                  height = "200px"),
        title = "Code preview",
        status = "success", solidHeader = TRUE, width = 12),

      # DATA: File preview box ----
      box(
        plotDownloadLink(id = "data_file_download"),

        tabsetPanel(id = "data_file_plot_tabs",
                    tabPanel("Static Plot", value = "gg",
                             plotOutput("data_file_plot", height="500px", width = "100%")),
                    tabPanel("Interactive Plot", value = "i",
                             plotlyOutput("data_file_iplot", height="500px", width = "100%"))
        ),

        title = "Data file quick view",
        status = "info", solidHeader = TRUE, width = 12)


    ) # / tabItem

  ) %>% dashboardBody()

)
