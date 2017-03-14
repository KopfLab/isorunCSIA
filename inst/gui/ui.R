# settings
modes <- read_excel(.settings_file, sheet = "modes")

# Define UI that plots the isotope label enrichment
ui <- dashboardPage(

  # SKIN ----
  skin = "blue",

  # HEADER ----
  dashboardHeader(title = "Isorun CSIA"),

  # SIDEBAR ----
  sidebarMenu(

    menuItem("Welcome", tabName = "welcome", icon = icon("info")),
    menuItem("Instrument", tabName = "instrument", icon = icon("cog")),
    menuItem("Tuning", tabName = "tuning", icon = icon("music")),
    menuItem("Standards", tabName = "standards", icon = icon("check")),
    menuItem("Data", tabName = "data", icon = icon("pie-chart")),
    menuItem("Scans", tabName = "scans", icon = icon("bar-chart"), selected = TRUE),
    menuItem("Settings", tabName = "settings", icon = icon("wrench")),
    radioButtons("mode", label = "Mode", choices = modes$Mode),

    # STYLESHEET ----
    tags$head(
      tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 16px; }")),
      tags$style(HTML(".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }")),
      tags$style(HTML(".sidebar {height:2000px}")), # FIXME: make this dynamically long enough
      tags$style(HTML(".box-body {padding-top: 15px; padding-bottom: 0px;}"))
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

          br(),
          box(title = NULL, collapsible = FALSE, solidHeader = FALSE, width = 12,
              column(4, div(align = "left", textInput("user", NULL, placeholder = "Please enter your name"))),
              column(8, div(align = "right", h4(actionLink("instrument_new_clear", "Clear all", icon = icon("rotate-left")))))
          ),

          box(title = "Background", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning", # FIXME: change color with save status!
              historyInfoInput(id = "background"),
              historyArchiveButton(id = "background"),
              modalFileSelectorInput(id = "full_scan_files", open_label = "Save full scan file", link_wrapper = function(dlg_link) {
                h4(dlg_link, textOutput("full_scan_file", inline = TRUE))
              }, allow_upload = FALSE)
          ),

          box(title = "Sensitivity & Peak shape", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning",
              historyInfoInput(id = "sensitivity"),
              historyArchiveButton(id = "sensitivity"),
              modalFileSelectorInput(id = "peak_shape_files", open_label = "Save peak shape file", link_wrapper = function(dlg_link) {
                h4(dlg_link, textOutput("peak_shape_file", inline = TRUE))
              }, allow_upload = FALSE)
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
          br(),
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

    tabItem(
      tabName = "data",

      br(),
      box(
        title = "Relevant data files", collapsible = TRUE,
        status = "success", solidHeader = TRUE, width = 12,

        selectInput("data_files_list", label = NULL, multiple = TRUE, size = 8, selectize = FALSE,
                    choices = c(),
                    selected = c()),
        tags$script(sprintf( # double click activation
          " $('#%s').on('dblclick', function(){
          var obj = $('select#%s')[0];
          Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
          })", "data_files_list", "data_files_list", "data_file_list_dblclick")),

        column(4, div(align = "left",
            modalFileSelectorInput(id = "data_files_select",
                                   open_label = "Add data files", close_label = "Add",
                                   link_wrapper = h4, allow_upload = FALSE))),
        column(8, div(align = "right",
            h4(
              actionLink("data_files_remove", "Remove", icon = icon("remove")), " | ",
              actionLink("data_files_export", "Export Excel", icon = icon("cloud-download")), " |",
              bsTooltip("data_files_export", "Export the selected files to excel"),
              downloadLink("data_files_download", class = NULL, icon("file-zip-o"), "Download"), " |",
              bsTooltip("data_files_download", "Download data files as a zip archieve"),
              actionLink("data_files_load", "Load", icon = icon("bar-chart")),
              bsTooltip("data_files_load", "Load the selected files together")
            )),
            # Excel export modal dialog
            bsModal("data_files_export_dialog", "Export to Excel",
                    "data_files_export", size = "small",
                    textInput("data_files_export_name", "Filename:", ""),
                    selectInput("data_files_export_columns", "Columns to export:", choices = c(),
                                multiple = TRUE, selectize = FALSE, size = 10, width = "100%"),
                    downloadButton("data_files_export_save", "Save", icon("save"))
            )
        )
      ),

      box(
        title = "Chromatograms", collapsible = TRUE,
        status = "warning", solidHeader = TRUE, width = 12,
        plotDownloadLink(id = "data_plot_download"),
        tabsetPanel(
          id = "data_plot_tabs", selected = "i",
          tabPanel("Interactive Plot", value = "i",
                   plotlyOutput("data_iplot", height="500px", width = "100%")),
          tabPanel("Static Plot", value = "gg",
                   plotOutput("data_plot", height="500px", width = "100%"))
        )
      ),

      box(
        title = "Data Table", collapsible = TRUE,
        status = "info", solidHeader = TRUE, width = 12,
        guiDataTable("data_files_table")
      )

    ), # / DATA tabItem


    # SCANS ----

    tabItem(
      tabName = "scans",

      br(),
      box(
        title = "Scan files", collapsible = TRUE,
        status = "success", solidHeader = TRUE, width = 12,

        selectInput("scan_files_list", label = NULL, multiple = TRUE, size = 8, selectize = FALSE,
                    choices = c(),
                    selected = c()),
        tags$script(sprintf( # double click activation
          " $('#%s').on('dblclick', function(){
          var obj = $('select#%s')[0];
          Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
          })", "scan_files_list", "scan_files_list", "scan_file_list_dblclick")),

        column(4, div(align = "left",
                      modalFileSelectorInput(id = "scan_files_select",
                                             open_label = "Add scan files", close_label = "Add",
                                             link_wrapper = h4, allow_upload = FALSE))),
        column(8, div(align = "right",
                      h4(
                        downloadLink("scan_files_download", class = NULL, icon("file-zip-o"), "Download"), " |",
                        bsTooltip("scan_files_download", "Download scan files as a zip archieve"),
                        actionLink("scan_files_load", "Load", icon = icon("bar-chart")),
                        bsTooltip("scan_files_load", "Load the selected scans together")
                      )))
      ), # // end BOX

      box(
        title = "Scans", collapsible = TRUE,
        status = "warning", solidHeader = TRUE, width = 12,
        plotDownloadLink(id = "scans_plot_download"),
        tabsetPanel(
          id = "scans_plot_tabs", selected = "i",
          tabPanel("Interactive Plot", value = "i",
                   plotlyOutput("scans_iplot", height="600px", width = "100%")),
          tabPanel("Static Plot", value = "gg",
                   plotOutput("scans_plot", height="600px", width = "100%"))
        )
      )
  ), # / SCANS tabItem


    # SETTINGS ----

    # TUNING:  ----
    tabItem(
      tabName = "tuning",

      br(),
      box(title = NULL, collapsible = FALSE, solidHeader = FALSE, width = 12,
          column(4, div(align = "left", textInput("tuning_user", NULL, placeholder = "Please enter your name"))),
          column(8, div(align = "right", h4(actionLink("tuning_new_clear", "Clear all", icon = icon("rotate-left")))))
      ),

      box(title = "Tuning parameters", collapsible = TRUE, solidHeader = TRUE, width = 12, status = "warning",
          historyInfoInput(id = "tuning"),
          historyArchiveButton(id = "tuning"),
          modalFileSelectorInput(id = "tuning_peak_shape_files",
                                 open_label = "Save peak shape file", link_wrapper = function(dlg_link) {
            h4(dlg_link, textOutput("tuning_peak_shape_file", inline = TRUE))
          }, allow_upload = FALSE)
      )

    ), # / tabItem

    # TUNING: Analysis ----
    tabItem(
      tabName = "tuning_analyze", h2("hello"))

  ) %>% dashboardBody()

)
