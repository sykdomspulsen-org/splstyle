#' Epicurve
#' @param granularity_time day or isoweek
#' epicurve(d = d_day, granularity_time = "day")
#' epicurve(d = d_week, granularity_time = "isoyearweek")
#' @export
epicurve <- function(d, granularity_time = "day") {

  # d <- spltidy::generate_test_data() %>% setnames("deaths_n", "cases_n")
  # d[, date := as.Date("2022-03-01")]
  # granularity_time <- "day"
  # granularity_time <- "isoyearweek"

  if(granularity_time == "day"){
    q <- ggplot(d, aes(x = date, y = cases_n))
    q <- q + scale_x_date("Date")
  } else{
    q <- ggplot(d, aes(x = isoyearweek, y = cases_n))
    q <- q + scale_x_discrete("Isoweek")
  }

  q <- q + geom_col(fill = splstyle::base_color, width = 0.8)
  q <- q + scale_y_continuous("Number of reported cases",
                              expand = expansion(mult = c(0, 0.1))
  )
  q <- q + labs(caption = fhi_caption())
  q <- q + splstyle::theme_fhi_lines_horizontal()
  q
}
