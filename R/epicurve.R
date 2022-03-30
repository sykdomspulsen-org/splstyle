#' Epicurve
#' @param x Dataset
#' @param granularity_time day or isoweek
#' @param ... Arguments
#' @examples
#' epicurve(x = d_day, granularity_time = "day")
#' epicurve(x = d_week, granularity_time = "isoyearweek")
#' @export
epicurve <- function(x, granularity_time = "day", ...) {
  UseMethod("epicurve", x)
}

#' Epicurve
#' @param x Dataset
#' @param granularity_time day or isoweek
#' @param ... Arguments
#' @examples
#' d <- spltidy::generate_test_data() %>% setnames("deaths_n", "cases_n")
#' epicurve(x = d_day, granularity_time = "day")
#' epicurve(x = d, granularity_time = "isoweek")
#' @export
epicurve.default <- function(x,
                             facet_wrap = NULL,
                             facet_ncol = NULL,
                             var_x = "date",
                             var_y = "cases_n",
                             breaks_x = NULL,
                             lab_x = NULL,
                             lab_y = NULL,
                             lab_main = NULL,
                             lab_sub = NULL,
                             lab_caption = splstyle::fhi_caption(),
                             format_y = splstyle::format_nor_num_0,
                             ...) {

  # lab_y = "Number of reported deaths"
  # var_y = "N"
  # facet_wrap = "location_code"
  # facet_ncol = 2
  # breaks_x = splstyle::every_nth(5)

  stopifnot(var_x %in% c("date", "isoyearweek"))
  if(FALSE){
    stop("this is an error message")
  }

  # dots <- list(...)

  q <- ggplot(x, aes(x = get(var_x), y = get(var_y)))

  if(var_x == "date"){
    q <- q + scale_x_date(name = lab_x)
  } else{
    q <- q + scale_x_discrete(name = lab_x, breaks = breaks_x)
  }

  q <- q + geom_col(fill = splstyle::base_color, width = 0.8)

  q <- q + lemon::facet_rep_wrap(~get(facet_wrap), repeat.tick.labels = "y", ncol = facet_ncol)


  q <- q + scale_y_continuous(name = lab_y,
                              expand = expansion(mult = c(0, 0.1)),
                              breaks = splstyle::pretty_breaks(5),
                              labels = format_y
  )
  q <- q + labs(caption = fhi_caption())
  q <- q + labs(title = lab_main,
                subtitle = lab_sub,
                caption = lab_caption)
  q <- q + splstyle::theme_fhi_lines_horizontal()
  q
}



test_data <- function(var_x = NULL) {
  set.seed(4)
  dates <- sample(seq.Date(as.Date("2018-01-01"),
                           as.Date("2018-03-08"), 1),
                  20000,
                  replace = T)
  d <- expand.grid(
    # location_code = "norge",
    location_code = unique(fhidata::norway_locations_b2020$county_code),
    date = dates
  )
  # Convert to data.table
  setDT(d)

  # print
  print(d)

  # Convert to data.table
  setDT(d)

  # aggregate
  d <- d[,
         .(
           N = .N
         ),
         keyby = .(
           location_code,
           date
         )
  ]
  # aggregated daily dataset that does not contain days with 0 cases
  print(d)

  # create skeleton
  skeleton <- data.table(expand.grid(
    # location_code = "norge",
    location_code = unique(fhidata::norway_locations_b2020$county_code),
    date = seq.Date(min(d$date), max(d$date), 1)
  ))

  # merge the two datasets together
  d <- merge(d, skeleton, by=c("location_code", "date"), all=T)

  # Fill in 'missing' Ns with 0
  d[is.na(N), N := 0]

  # Now you have a clean aggregated daily dataset that contains days with 0 cases!
  print(d)

  if(!is.null(var_x)){
    # create 3 new variables:
    d[, isoyearweek := fhi::isoyearweek(date)]

    # aggregate down to weekly level
    w <- d[,
           .(
             N = sum(N)
           ),
           keyby = .(
             location_code,
             isoyearweek
           )
    ]
    print(w)
    return(w)
  }

  return(d)
}
