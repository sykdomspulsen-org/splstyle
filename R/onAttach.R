#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("splstyle", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "splstyle ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/splstyle"
  ))
}
