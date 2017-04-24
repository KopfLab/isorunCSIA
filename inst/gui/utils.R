#' UTILITY FUNCTIONS

get_setting <- function(data, variable, default = NULL, mode = NULL) {
  setting <- filter(data, Variable == variable)
  if (!is.null(mode)) setting <- filter(setting, grepl(mode, Mode, fixed = T))
  if (nrow(setting) == 0) return(default)
  else return(setting$Value)
}

#' splits string by | to make vector, interprets x=y entries with x as entry name
vectorize_setting_string <- function(x) {
  strsplit(x, split="|", fixed = T)[[1]] %>%
    lapply(function(i) {
      name_col_split <- strsplit(i, split="=", fixed = T)[[1]]
      if (length(name_col_split) == 2) list(name_col_split[2]) %>% setNames(name_col_split[1])
      else if (length(name_col_split) == 1) list(name_col_split[1]) %>% setNames(name_col_split[1])
    }) %>%
    unlist()
}

#' error round (not used yet)
round_to_err <- function(x, sd, sig_digits = 1, sep = "+-") {
  if(!is.numeric(x)) return("NA")
  stopifnot(length(x) == length(sd))
  n_deci <- nchar(sub('^[0-9]*\\.([0-9]*)0*$', '\\1', as.character(signif(sd, sig_digits))))
  n_deci[(x %% 1) == 0] <- 0 # numbers w/o any decimal places
  ifelse(is.na(x), "NA", paste0(round(x, n_deci), sep, round(sd, n_deci)))
}

#' get mass trace data (could be an exported function?)
get_iso_mass_traces <- function(files) {
  data <-
    files %>%
    lapply(function(i) mutate(i$get_mass_data(melt = T)[c("time", "signal", "variable")], file = i$filename)) %>%
    bind_rows() %>%
    as_data_frame()
  if (nrow(data) == 0) return(NULL)
  else return(data)
}

#' get table data (could be an export function?)
get_iso_data_tables <- function(files){
  if (length(files) == 0) return(NULL)
  data <- files %>%
    lapply(function(file) {
      dt <- file$get_data_table() %>% arrange(row_number())
      rows_set1 <- dt %>% filter(!is.na(`Nr.`))
      rows_set2 <- dt %>% filter(is.na(`Nr.`))
      if (nrow(rows_set2) == 0) {
        data <- dt
      } else {
        cols1 <- names(rows_set1)[sapply(rows_set1, function(col) !all(is.na(col)))]
        cols2 <- names(rows_set2)[sapply(rows_set2, function(col) !all(is.na(col)))]
        cols2 <- cols2[!cols2 %in% cols1] # avoid duplicates
        data <- cbind(rows_set1[cols1], rows_set2[cols2])
      }
      mutate(data, File = file$filename)[c("File", names(dt))]
    }) %>%
    bind_rows() %>%
    as_data_frame()
  if (nrow(data) == 0) return(NULL)
  else {
    # sanitize column names (remove trailing white spaces)
    names(data) <- trimws(names(data))
    return(data)
  }
}

#' load isotope data (could be an exported function except for progress)
load_iso_data <- function(files, loaded = c(), root = ".", quiet = FALSE) {

  # check which files are overlaoded
  over_loaded <- setdiff(names(loaded), files)
  if ( (n <- length(over_loaded)) > 0) {
    message("INFO: unloading ", n, " loaded iso files (no longer needed)")
    loaded <- loaded[intersect(names(loaded), files)]
  }

  # check which files have not been loaded yet / are overloaded
  not_loaded_yet <- setdiff(files, names(loaded))

  if ( (n <- length(not_loaded_yet)) > 0) {
    message("INFO: loading ", n, " iso files")

    # read isodat files
    iso_files <- list()
    withProgress(message = 'Loading data...', value = 0, {
      for (file in not_loaded_yet) {
        incProgress(1/n, detail = paste0("Reading ", file, " ..."))
        tryCatch({
          iso_file <- list(isoread::isoread(file.path(root, file), quiet = quiet)) %>% setNames(file)
          iso_files <- c(iso_files, iso_file)
        },
        error = function(e) message("ERROR: encountered error while reading file ", file, ": ", e$message),
        warning = function(w) message("WARNING: encountered warning while reading file ", file, ": ", w$message))
      }
    })
    return(c(loaded, iso_files))
  } else {
    return(loaded)
  }
}

#' generates data summary table
#' this function is different from isorunN2Os generated_data_table in that it allows arbitrary summarizing functions andis focused on renaming existing columns and uses standard evaluation
#' @param data the data frame
#' @param functions list of functions generated with call to funs()
#' @param cols name of the columns to summarize, if named will be used to rename columns (NOTE: all _ will be replaced with space)
#' @param n_col number column name (if NULL is omitted from summary)
#' @param col_fun_sep the separator between column and function name
#' @param summary_row if provided, adds a summary row and puts the provided value into all summary columns
#' @example
#' generate_summary_table(
#'    group_by(iris, Species),
#'    fun = funs(Average = mean, Sigma = sd),
#'    cols = c(Length = "Sepal.Length", Width = "Sepal.Width"),
#'    n_col = "# observations",
#'    col_fun_sep = " | ")
generate_summary_table <- function(data, functions, cols = c(), n_col = NULL, col_fun_sep = " ", summary_row = NULL) {
  if (!is(functions, "fun_list")) stop("need to provide a funs(...) list of functions", call. = FALSE)
  if (length(functions) == 0) stop("need to specify at least one function", call. = FALSE)
  if (length(cols) == 0) stop("need to specify at least one column", call. = FALSE)

  # what groups are used in original df (need to account for `` if present)
  data_groups <- sub("^`([^`]*)`$", "\\1", as.character(groups(data)))

  # make sure all columns are named
  if (is.null(names(cols))) names(cols) <- cols
  names(cols)[names(cols) == ""] <-  cols[names(cols) == ""]

  # make sure all referenced columns exist
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("cannot generate summary table, some of the requested columns do not exist: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # rename+select columns
  rename_cols <- paste0("`", cols, "`") %>% setNames(names(cols))
  data <- suppressMessages(select_(data, .dots = rename_cols))

  # n() column
  if (!is.null(n_col)) {
    df <- data %>% mutate(.n = n()) %>% rename_(.dots = c(".n") %>% setNames(n_col))
    df <- df %>% group_by_(.dots = sprintf("`%s`", n_col), add = TRUE)
  } else {
    df <- data
  }

  # summarize columns
  sum_cols <- sprintf("`%s`", names(cols))
  sum_table <- df %>% summarize_at(sum_cols, functions) %>% ungroup()

  # total summary row
  final_row <- data_frame()
  if (!is.null(summary_row)) {
    grp_mutates <- sapply(data_groups, function(grp) list(interp(~x, x = summary_row)))
    final_row <- data %>% ungroup() %>%
      # overall n
      mutate(.n = n()) %>% rename_(.dots = c(".n") %>% setNames(n_col)) %>%
      # summarize all
      group_by_(.dots = sprintf("`%s`", n_col)) %>%
      summarize_at(sum_cols, functions, na.rm = TRUE) %>% ungroup() %>%
      # grouping columns
      mutate_(.dots = grp_mutates)
  }
  sum_table <- suppressWarnings(bind_rows(sum_table, final_row))

  # rename summarized columns with col_func_sep select final columns
  sum_fun_cols <-
    sprintf("`%s`", paste0(rep(names(cols), each = length(functions)), "_", names(functions))) %>%
    setNames(paste0(rep(names(cols), each = length(functions)), col_fun_sep, names(functions)))
  sum_table <- sum_table %>% rename_(.dots = sum_fun_cols)

  # final column order and selection
  final_cols <- c(as.character(groups(df)), sprintf("`%s`", names(sum_fun_cols)))
  sum_table <- sum_table %>% select_(.dots = final_cols)

  return(sum_table)
}
