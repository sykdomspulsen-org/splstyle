#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "splstyle ",
    utils::packageDescription("fhidata")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/splstyle"
  ))
}
