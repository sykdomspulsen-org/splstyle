#' set_x_axis_vertical
#' @import ggplot2
#' @export
set_x_axis_vertical <- function() {
  return(theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
}
