library(shinyBS)

#' fileSelector
#' @param allow_upload whether to allow upload
fileSelectorInput <- function(id, allow_upload = FALSE, upload_label = NULL) {
  ns <- NS(id)
  dialog_tags <-
    tagList(

      uiOutput(ns("tab_path")),
      uiOutput(ns("content_list")),
      if (allow_upload) fileInput(ns("upload"), upload_label),

      # reset selection when switching tabs
      tags$script(
        "
        $('#%s').on('click', function(){
          Shiny.onInputChange('%s', Math.random());
        })" %>%
        sprintf(ns("tab_path"), ns("tab_click"))),

      # enable sub folder selection on double click
      tags$script(
        "
        $('#%s').on('dblclick', function(){
          var obj = $('select#%s')[0];
          Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
          Shiny.onInputChange('%s', Math.random());
        })" %>%
        sprintf(ns("content_list"), ns("content_list"),
                ns("content_double_click_item"), ns("content_double_click")))
    )

  return(dialog_tags)
}


#' @param root directory
#' @param root_name the root directory name
#' @param sort_desc whether to sort in descending order
#' @param pattern regexp file selection pattern
#' @param size number of rows in the selection box
fileSelector <- function(input, output, session,
                         root, root_name = basename(root),
                         sort_desc = FALSE, pattern = NULL,
                         size = 8) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    upload_counter = 1,
    last_double_click_item = "",
    current_directory = root,
    current_directory_content = NULL,
    content_list = c()
    )

  # uploads
  observe({
    # upload (expand zip)
    upload <- input$upload
    if (is.null(upload)) return()
    ext <- stringr::str_match(basename(upload$name), "\\.(\\w+$)")[1,2]
    target <- isolate(values$current_directory)
    if (!is.na(ext) && ext == "zip") upload$datapath %>% unzip(exdir = target)
    else upload$datapath %>% file.copy(to = file.path(target, upload$name))

    # info and update trigger
    counter <- isolate(values$upload_counter)
    sprintf("Upload #%d complete: %s", counter + 1, file.path(target, upload$name)) %>%
      message()
    values$upload_counter <- counter + 1
  })

  # update reactive value last_double_click_item
  observe({
    validate(need(input$content_double_click, message = FALSE)) # only run if set
    input$content_double_click
    values$last_double_click_item <- isolate(input$content_double_click_item)
  })

  # update reactive value current_directory
  observe({

    # refresh whenever directory tab is clicked or subfolder is double clicked
    input$tab_click
    input$content_double_click

    # isolate everything after
    isolate({
      # base path (start at root if not set yet)
      path <- input$tab_path
      if (is.null(path)) path <- root

      # see if last double click item is a valid subdirectory
      subdir <- values$last_double_click_item
      if (!is.null(subdir) && subdir != "" &&
          dirname(subdir) == path && # make sure it's a subdirectory
          dir.exists(subdir) # make sure it's a directory
        ) {
        path <- subdir
        values$last_double_click_item <- "" # reset
      }

      # safety checks
      stopifnot(file.exists(path))
      if (!R.utils::isAbsolutePath(path)) stop("not an absolute path: ", path)

      # update reactive value
      if (path != values$current_directory) {
        values$current_directory <- path
        values$current_directory_content <- NULL
      }
    })
  })

  # update reactive value content_list
  observe({

      # trigger when current folder changes and check for updates in that folder every second
      invalidateLater(1000, session)
      path <- values$current_directory
      # just a quick content comparison (no full paths or anything)
      new_content <- c(list.dirs(path, rec = FALSE), list.files(path, pattern = pattern))
      old_content <- isolate(values$current_directory_content)
      if (!is.null(old_content) && setdiff(new_content, old_content) %>% length() == 0) {
        return() # okay, nothing changed
      }

      # something changed --> update contents list
      message("INFO: (Re)loading folder: ", path, " (NS: ", ns(NULL),")")
      values$current_directory_content <- new_content
      folders <- list.dirs(path, rec=FALSE, full.names = T) # w/ full names
      files <- setdiff(list.files(path, full.names = T, pattern = pattern), folders)

      # sort in descending order if asked for
      if (sort_desc) {
        folders <- rev(folders)
        files <- rev(files)
      }

      # combine
      values$content_list <-
        setNames(
          c(folders, files),
          c(sprintf("[ %s ]", folders %>% sapply(basename)), files %>% sapply(basename))
        )
    })

  # generate path tabs
  output$tab_path <- renderUI({
    tmp_path <- values$current_directory
    parents <- list(id = ns("tab_path"), selected = tmp_path)
    while (tmp_path != dirname(root)){
      if (tmp_path == root)
        parent <- root_name
      else
        parent <- basename(tmp_path)
      parents <- c(parents, list(tabPanel(parent, value = tmp_path)))
      tmp_path <- dirname(tmp_path)
    }
    do.call(tabsetPanel, args = parents[length(parents):1])
  })

  # generate folder content listing
  output$content_list <- renderUI({
    selectInput(ns("content_list"), NULL, width = "100%",
                size = size, selectize = F, multiple = T,
                values$content_list)
  })

  # return both the current path and the selected folder contents
  list(
    path = reactive(values$current_directory),
    selection = reactive(input$content_list),
    double_click = reactive(values$last_double_click_item)
  )
}

