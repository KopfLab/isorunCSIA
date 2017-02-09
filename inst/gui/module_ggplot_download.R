library(shinyjs)
library(ggplot2)

#' fileSelector
#' @param allow_upload whether to allow upload
#' @note requires use of shinyjs (shinyjs::useShinyjs() earlier on the page)
plotDownloadLink <- function(id, label = "Save plot", default_filename = "plot.pdf") {
  ns <- NS(id)
  tagList(

    # DOWNLOAD LINK ----
    div(align = "right",
        actionLink(ns("download_plot_link"), label, icon = icon("download")),
        bsTooltip(ns("download_plot_link"), "Download the plot as a PDF")
    ),

    # MODAL SAVE DIALOG ----
    bsModal(ns("save_dialog"), label, ns("download_plot_link"), size = "small",
            textInput(ns("save_name"), "Filename:", default_filename),
            numericInput(ns("save_width"), "Width [inches]:", 12),
            numericInput(ns("save_height"), "Height [inches]:", 8),
            downloadButton(ns("save_plot"), "Save", icon("save"))
    )
  )
}

#' @param plot_func reactive function generating the plot
#' @param filename_func reactive function returning the default plot name
plotDownloadDialog <- function(input, output, session, plot_func, filename_func) {

  # update default filename
  observe({
    updateTextInput(session, "save_name", value = filename_func())
  })

  # actual safe handler
  output$save_plot <- downloadHandler(
    filename = function() { isolate(input$save_name) },
    content = function(file) {
      message("saving")
      device <- function(..., version="1.4") grDevices::pdf(..., version=version)
      ggsave(file = file, plot = plot_func(), width = isolate(input$save_width), height = isolate(input$save_height), device = device)
    })

}
