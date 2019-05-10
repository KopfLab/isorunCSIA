.onLoad <- function(libname, pkgname) {
  # set default package options
  default_options <- list(
    isorunCSIA.con = NULL,
    isorunCSIA.instrument_id = NULL,
    isorunCSIA.quiet = FALSE,
    isorunCSIA.debug = FALSE
  )
  options(default_options)
  invisible()
}
