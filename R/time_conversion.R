# today to isoyear/week ====
#' isoyear_c
#' @param x The date of interest
#' @export
isoyear_c <- function(x = lubridate::today()) {
  yr <- format.Date(x, "%G")
  return(yr)
}

#' isoyear_n
#' @param x The date of interest
#' @export
isoyear_n <- function(x = lubridate::today()) {
  yr <- as.numeric(isoyear_c(x))
  return(yr)
}

#' isoweek_c
#' @param x The date of interest
#' @export
isoweek_c <- function(x = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(x, "%V")
  return(wk)
}

#' isoweek_n
#' @param x The date of interest
#' @export
isoweek_n <- function(x = lubridate::today()) {
  wk <- as.numeric(isoweek_c(x))
  return(wk)
}

#' isoyearweek_c
#' date to isoyearweek_c (character)
#' @param x Date
#' @export
isoyearweek_c <- function(x = lubridate::today()){
  return(paste0(isoyear_c(x),"-",isoweek_c(x)))
}





# week to season/season to week -----
#' week_to_seasonweek_n
#' Natural week to season week. Season week 1 is natural week 30.
#' @param week Natural week in a year
#' @export
week_to_seasonweek_n <- function(week){
  # take both char/n in input

  # real week 30 is the start of season, week 1
  # original: fhi::x(20)
  retval <- week
  retval[week >= 30] <- week[week >= 30] - 29
  retval[week < 30] <- week[week < 30] + 23
  retval[week == 53] <- 23.5

  return(retval)
}

#' seasonweek_to_week_c
#' Season week to natural week. Season week 1 is natural week 30.
#' @param seasonweek Season week in a year
#' @export
seasonweek_to_week_c <- function(seasonweek){
  # influenza week 1 (x) is real week 30

  retval <- seasonweek
  retval[seasonweek <= 23] <- seasonweek[seasonweek <= 23] + 29
  retval[seasonweek > 23] <- seasonweek[seasonweek >23] - 23
  retval[seasonweek == 23.5] <- 53
  # return double digit: 01, 09, 10, 11
  retval <- formatC(retval, width=2, flag="0")

  return(retval)
}

#' seasonweek_to_week_n
#' Season week to natural week. Season week 1 is natural week 30.
#' @param seasonweek Season week in a year
#' @export
seasonweek_to_week_n <- function(seasonweek){
  # influenza week 1 (x) is real week 30

  retval <- seasonweek
  retval[seasonweek <= 23] <- seasonweek[seasonweek <= 23] + 29
  retval[seasonweek > 23] <- seasonweek[seasonweek >23] - 23
  retval[seasonweek == 23.5] <- 53
  return(retval)
}





# yrwk to year/week ====

#' isoyearweek_to_year_n
#' isoyearweek to year (numeric)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_year_n <- function(yrwk){
  year_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]}) %>% as.numeric()
  return(year_n)
}


#' isoyearweek_to_year_c
#' isoyearweek to year (character)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_year_c <- function(yrwk){
  year_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]})
  return(year_c)
}

#' isoyearweek_to_week_n
#' isoyearweek to week (numeric)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_week_n <- function(yrwk){
  week_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]}) %>% as.numeric()
  return(week_n)
}


#' isoyearweek_to_week_c
#' isoyearweek to week (character)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_week_c <- function(yrwk){
  week_c <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[2]})
  return(week_c)
}

keep_sundays_and_latest_date_internal <- function(dates, format = "Uke isoweek_c-1/isoweek_c", keep_delete = TRUE, keep_latest_date = TRUE){
  stopifnot(format %in% c("isoyearweek_c", "Uke isoweek_c", "isoyearweek_c-1/isoyearweek_c", "Uke isoweek_c-1/isoweek_c", "date"))
  values <- data.table(
    date = dates,
    order = 1:length(dates),
    isoyearweek = isoyearweek_c(dates)
  )
  setorder(values, -date)
  values[, n := 1:.N, by=.(isoyearweek)]
  setorder(values, date)
  values[, time_description := as.character(date)]
  if(keep_latest_date){
    values[time_description != max(time_description), time_description := "delete"]
  } else {
    values[, time_description := "delete"]
  }
  if(format == "isoyearweek_c"){
    values[
      date %in% fhidata::world_dates_isoyearweek$sun,
      time_description := paste0(isoyearweek_c(date))
    ]
  } else if(format == "Uke isoweek_c"){
    values[
      date %in% fhidata::world_dates_isoyearweek$sun,
      time_description := paste0("Uke ",isoweek_c(date))
    ]
  } else if(format == "isoyearweek_c-1/isoyearweek_c"){
    values[
      date %in% fhidata::world_dates_isoyearweek$sun,
      time_description := paste0(isoyearweek_c(date-7), "/", isoyearweek_c(date))
    ]
  } else if(format == "Uke isoweek_c-1/isoweek_c"){
    values[
      date %in% fhidata::world_dates_isoyearweek$sun,
      time_description := paste0("Uke ",isoweek_c(date-7),"/", isoweek_c(date))
    ]
  } else if(format == "date"){
    values[
      date %in% fhidata::world_dates_isoyearweek$sun,
      time_description := as.character(date)
    ]
  }
  levels <- unique(c("delete", values$time_description))
  setorder(values, order)

  retval <- factor(values$time_description, levels = levels)

  if(!keep_delete){
    retval <- as.character(retval)
    retval <- retval[retval!="delete"]
  }
  return(retval)
}

#' Keeps sundays and latest date
#' If you provide a vector of dates, this function will keep the sundays
#' and the latest date
#' @param dates Vector of dates
#' @param format Choose between: "isoyearweek_c", "Uke isoweek_c", "isoyearweek_c-1/isoyearweek_c", "Uke isoweek_c-1/isoweek_c", "date"
#' @param keep_delete Keep everything in the same format as provided
#' @examples
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "isoyearweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "Uke isoweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "isoyearweek_c-1/isoyearweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "Uke isoweek_c-1/isoweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "date",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays_and_latest_date(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "date",
#'   keep_delete = FALSE
#' )
#' @export
keep_sundays_and_latest_date <- function(dates, format = "Uke isoweek_c-1/isoweek_c", keep_delete = TRUE){
  retval <- keep_sundays_and_latest_date_internal(
    dates = dates,
    format = format,
    keep_delete = keep_delete,
    keep_latest_date = TRUE
  )
  return(retval)
}

#' Keeps sundays
#' If you provide a vector of dates, this function will keep the sundays
#' and the latest date
#' @param dates Vector of dates
#' @param format Choose between: "isoyearweek_c", "Uke isoweek_c", "isoyearweek_c-1/isoyearweek_c", "Uke isoweek_c-1/isoweek_c", "date"
#' @param keep_delete Keep everything in the same format as provided
#' @examples
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "isoyearweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "Uke isoweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "isoyearweek_c-1/isoyearweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "Uke isoweek_c-1/isoweek_c",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "date",
#'   keep_delete = TRUE
#' )
#'
#' splstyle::keep_sundays(
#'   dates = seq.Date(as.Date("2020-01-01"), as.Date("2020-02-01"), by=1),
#'   format = "date",
#'   keep_delete = FALSE
#' )
#' @export
keep_sundays <- function(dates, format = "Uke isoweek_c-1/isoweek_c", keep_delete = TRUE){
  retval <- keep_sundays_and_latest_date_internal(
    dates = dates,
    format = format,
    keep_delete = keep_delete,
    keep_latest_date = FALSE
  )
  return(retval)
}



