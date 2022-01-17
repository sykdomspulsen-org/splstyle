#' every_nth
#' Useful for plotting every nth discrete value in ggplot2
#' @param n nth discrete value
#' @examples
#' \dontrun{
#' scale_x_discrete(NULL, breaks = every_nth(n = 2))
#' }
#' @export
every_nth <- function(n) {
  force(n)
  return(function(x) {
    # old:  x[c(TRUE, rep(FALSE, n - 1))]: prints T,F,F, T,F,F
    # alt1: x[c(rep(FALSE, n - 1), TRUE)]: this also prints F,F,T, F,F,T
    rev(rev(x)[c(TRUE, rep(FALSE, n - 1))]) # prints c(2,5)
  })
}
