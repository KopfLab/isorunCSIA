## server side data table that is easy to update while retaining user settings / recreate new if new data is called for
# only works in server side mode, sorry (the replaceData does not work client-side)

library(dplyr)
library(DT)

#' gui DT
#' @param ... all parameters are passed to dataTableOutput
guiDataTable <- function(id, allow_upload = FALSE, upload_label = NULL) {
  ns <- NS(id)
  DT::dataTableOutput(ns("data_table"))
}

#' server DT
#' @param rownames default passed onto datatable function
#' @param selection default passed onto datatable function
#' @param filter default passed onto datatable function
#' @param sig_digitis number of significant digits used for all numeric formatting
#' @param extensions default passed onto datatable function
#' @param options default passed onto datatable function
#' @param ... additional params passed onto datatable function
serverDataTable <- function(input, output, session,
                            rownames = FALSE, selection = "none", filter = "top", # data table defaults
                            sig_digits = 3, # data table default numeric formatting
                            extensions = c("Buttons", "KeyTable"), # data table defaults
                            options = list( # data table defaults
                              dom = "Bltpi", # Buttons, Length, Table, Pagniation, Information
                              pageLength = 5, lengthMenu = c(5, 10, 15, 20, 50, 100), # paging menu
                              keys = TRUE, #KeyTable extension for excel like navigation
                              scrollX = TRUE, # automatic x scrolling for large table fits
                              buttons = list(list(extend = "colvis")) # Buttons - allow column selection for all
                            ), ...) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    data = NULL,
    new_table_data = NULL
  )

  # update data function
  update_data <- function(data) {
    if (is.null(values$data) || !setequal(names(values$data), names(data))) {
      values$new_table_data <- data
    } else {
      values$data <- data
    }
  }

  # render data table
  output$data_table <- DT::renderDataTable({
    req(values$new_table_data)
    message("INFO: Generating new data table for namespace '", ns(NULL), "'")
    isolate({
      values$data <- values$new_table_data
      # generate datatable
      dt <- datatable(values$data, rownames = rownames,
                      selection = selection, filter = filter,
                      #extensions = c("FixedHeader", "Scroller", "ColReorder", "Buttons", "KeyTable"), # potential extensions
                      extensions = extensions,
                      options = options)
      # format all numerics to 3 significant digits (SK note: make formatting more customizable)
      dt %>% formatSignif(names(values$data)[sapply(values$data, class) == "numeric"], digits = sig_digits) #
    })}, server = TRUE)

  # define data proxy
  proxy <- dataTableProxy(ns("data_table"), deferUntilFlush = FALSE)
  observe({
    req(values$data)
    message("INFO: Updating data table for namespace '", ns(NULL), "'")
    #replaceData(proxy, values$data, rownames = rownames, resetPaging = TRUE)
    # SK NOTE: this is a workaround because ajax needs session differently than dataTableProxy
    # https://github.com/rstudio/DT/issues/359
    dataTableAjax(session, values$data, rownames = rownames, outputId = "data_table")
    reloadData(proxy, resetPaging = TRUE)
    # SK NOTE: also, the reloadData will NOT reset the ranges on the column filters (this is probably a bug in DT)
  })

  # return functions
  list(
    update = update_data,
    rows_selected = reactive(input$data_table_rows_selected)
  )

}
