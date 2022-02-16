#' flextable_dailyreport_test
#' @param data dataset
#' @param width width
#' @param width_col column to change the width of (if any)
#' @param fontsize
#' @param fontname
#' @export

flextable_dailyreport_test <- function(data,
                                       width = NULL,
                                       width_col = NULL,
                                       fontsize,
                                       fontname = "calibri"
                                       ) {
  ft <- flextable::flextable(data)
  ft <- flextable::autofit(ft)

  if (!is.null(width)){
    ft <- flextable::width(ft, j = width_col, width = width)
  }

  ft <- flextable::fontsize(ft, size = fontsize, part = "all")
  ft <- flextable::font(ft, fontname = fontname, part = "all")
  ft <- flextable::align_text_col(ft, align = "right")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft

  return(ft)

}
