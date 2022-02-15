#' flextable_dailyreport_test
#' @param tab data.table
#' @param position Position
#' @export

flextable_dailyreport_test <- function(tab,
                                       width = NULL,
                                       width_col = NULL,
                                       fontsize,
                                       fontname = "calibri"
                                       ) {
  ft <- flextable::flextable(tab)
  ft <- flextable::autofit(ft)
  ft <- flextable::width(ft, j = width_col, width = width)
  # ft <- flextable::width(ft, j = 1, width = 1.55)
  ft <- flextable::fontsize(ft, size = fontsize, part = "all")
  ft <- flextable::font(ft, fontname = fontname, part = "all")
  ft <- flextable::align_text_col(ft, align = "right")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft

  return(ft)

}
