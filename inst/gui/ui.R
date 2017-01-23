
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
    radioButtons("element", label = "Element", choices = ELEMENTS),
    menuItem(
      "Tuning", tabName = "tunings", icon = icon("music"),
      menuSubItem("Select files", tabName = "tuning_files", icon = icon("files-o")),
      menuSubItem("Analysis", tabName = "tuning_analyze", icon = icon("bar-chart"))),

    # STYLESHEET ----
    tags$head(
      tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 16px; }")),
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
        id = "instrument_tabs", selected = "params",
        tabPanel("New", value = "new",

                 h4(actionLink("instrument_new_clear", "Clear all", icon = icon("rotate-left"))),

                 h2("Background"),
                 historyInfoInput(id = "background"),
                 historyArchiveButton(id = "background"),

                 h2("Sensitivity"),
                 historyInfoInput(id = "sensitivity"),
                 historyArchiveButton(id = "sensitivity"),
                 h4("Peak shape"),

                 h2("Instrument parameters"),
                 historyInfoInput(id = "parameters"),
                 historyArchiveButton(id = "parameters"),

                 h2("Isotopic stability"),
                 h4("ON/OFFs"),
                 h4("Linearity")

                 ),
        tabPanel(
          "Parameter History", value = "params",
          p(
            div(style="display:inline-block",
                selectInput("history_element", label = NULL, multiple = TRUE, width = "300px",
                            choices = ELEMENTS, selected = ELEMENTS),
                bsTooltip("history_element", "Which elements to consider", placement = "top", trigger = "hover")
            ),
            div(style="display:inline-block",
                selectInput("history_category", label = NULL, multiple = TRUE, width = "300px",
                            choices = names(HISTORY_FILES), selected = names(HISTORY_FILES)),
                bsTooltip("history_category", "Which parameter groups", placement = "top", trigger = "hover")
            ),
            #h4("Parameters to show", `for` = "history_variables"),
            selectInput("history_variables", label = NULL, multiple = TRUE, size = 5, selectize = FALSE, width = "600px",
                        choices = names(HISTORY_FILES), selected = names(HISTORY_FILES)),
            dateRangeInput("history_date_range", label = NULL,
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", separator = " to ")
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
      column(
        width = 12,
        fileSelectorInput(
          id = "tuning_files_local", allow_upload = TRUE,
          upload_label = 'Upload tuning files (individual or .zip archives)')) %>%
        fluidRow(),

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
