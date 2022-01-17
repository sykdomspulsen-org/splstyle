#' Formats captions with dates
#' @param source A source string
#' @param ... An unknown number of other sources to include
#' @param max_width The max_width of the caption
#' @export
caption_sources_with_dates <- function(source, ..., max_width = 180){
  dots <- list(...)
  #return(dots)
  #print(dots)

  n <- names(dots)
  vals <- unlist(dots)
  #print(n)
  #print(vals)

  retval <- paste0(source)
  if(length(dots) == 0) {
    retval <- stringi::stri_wrap(retval)
    return(retval)
  } else{
    retval <- paste0(retval, " / ")
    for(dot in n){
      retval <- paste0(retval, dot, " (", vals[dot], "), ")

    }
    retval <- stringi::stri_wrap(stringr::str_sub(retval, end = nchar(retval) - 2), width = max_width)
    return(retval)
  }
}



# caption_sources_with_dates(source = "Sykdomspulsen, FHI", "test"="ok")
# caption_sources_with_dates("Sykdomspulsen, FHI", "test"="ok", "testx"="osk")
# caption_sources_with_dates("Sykdomspulsen, FHI", "MSIS" = "2020-10-20", "sKUHR" = "2010-20-10", "test" = "testtest")
# caption_sources_with_dates("Sykdomspulsen, FHI")

