#' save_a4
#' @param p plot
#' @param filename filename
#' @param landscape landscape dimensions?
#' @param scaling_factor How much larger/smaller than A4?
#' @export
save_a4 <- function(p, filename, landscape = T, scaling_factor = 1) {
  if (landscape) {
    ggsave(
      filename,
      plot = p,
      width = 297 * scaling_factor,
      height = 210 * scaling_factor,
      units = "mm"
    )
  } else {
    ggsave(
      filename,
      plot = p,
      width = 210 * scaling_factor,
      height = 297 * scaling_factor,
      units = "mm"
    )
  }
}
