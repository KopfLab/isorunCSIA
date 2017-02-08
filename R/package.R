#' @title Run compound specific isotope analysis
#' @description Test
#' @name isorunCSIA
#' @docType package
#' @author Sebastian Kopf
#' @import dplyr ggplot2
#' @importFrom tidyr gather
#' @importFrom isoread isoread
#' @importFrom methods is
#' @importFrom stats setNames
#'
#' @include loading.R
#' @include gui.R
NULL


#' install the version of isorun CSIA from GitHub
#' @param ref which version to install, master (=newest) is the default
#' @export
update_isorunCSIA <- function(ref = "master") {
  on.exit({
    remove.packages("isorunCSIA")
    devtools::install_github("kopflab/isorunCSIA", ref = ref)
    message("\nInstallation complete: isorunCSIA version ", packageVersion("isorunCSIA"), "\n")
  })
}
