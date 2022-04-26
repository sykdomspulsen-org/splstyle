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
#' @param type "single", "stacked" or "dodged"
#' @param fill_var
#' @param fill_lab
#' @param facet_wrap What column in the dataset to spli
#' @param facet_ncol How many columns with graphs
#' @param var_x "date" or "isoyearweek"
#' @param var_y The name of the variable to use on the y-axis of the graph
#' @param breaks_x Use splstyle::every_nth() to choose how many ticks to show on the x-axis
#' @param lab_x The label of the x-axis
#' @param lab_y The label of the y-axis
#' @param lab_main The main title of the graph
#' @param lab_sub The subtitle of the graph
#' @param lab_caption If not specified, splstyle::fhi_caption() is used as the lab_caption.
#' @param format_y How the y-axis ticks should be formatted. For example splstyle::format_nor_num_0 or fhiplot::format_nor_perc_0
#' @param scale_y How to scale the y-axis if the graph is split with facet_wrap. Free or fixed.
#' @examples
#' x <- spltidy::generate_test_data()
#' epicurve(x[location_code == "county03"], type = "single", var_y = "deaths_n")
#' epicurve(x, type = "stacked", fill_var = "location_code", var_y = "deaths_n")
#' epicurve(x, type = "dodged", fill_var = "location_code", var_y = "deaths_n")
#' @export
epicurve.default <- function(x,
                             type = "single",
                             fill_var = NULL,
                             fill_lab = NULL,
                             facet_wrap = NULL,
                             facet_ncol = NULL,
                             var_x = "isoyearweek",
                             var_y,
                             breaks_x = NULL,
                             lab_x = NULL,
                             lab_y = NULL,
                             lab_main = NULL,
                             lab_sub = NULL,
                             lab_caption = splstyle::fhi_caption(),
                             format_y = splstyle::format_nor_num_0,
                             scale_y = "free",
                             ...) {

  # lab_y = "Number of reported deaths"
  # var_y = "N"
  # facet_wrap = "location_code"
  # facet_ncol = 2
  # breaks_x = splstyle::every_nth(2)
  # fill_var = "location_code"

  stopifnot(var_x %in% c("date", "isoyearweek"))
  stopifnot(type %in% c("single", "stacked", "dodged"))


  if(FALSE){
    stop("this is an error message")
  }

  # dots <- list(...)

  # if(fill_var == "location_code"){
  #   x[, location_name := splstyle::location_code_to_character(location_code)]
  # }


  if(type == "stacked"){
    q <- ggplot(x, aes_string(x = var_x, y = var_y, fill = fill_var))
    q <- q + geom_col(width = 0.8)
    q <- q + splstyle::scale_fill_fhi(fill_lab, palette="primary")
  } else if(type == "single"){
    q <- ggplot(x, aes_string(x = var_x, y = var_y))
    q <- q + geom_col(fill = splstyle::base_color, width = 0.8)
  } else if (type == "dodged") {
    q <- ggplot(x, aes_string(x = var_x, y = var_y, fill = fill_var))
    q <- q + geom_col(position = "dodge", width = 0.8)
    q <- q + splstyle::scale_fill_fhi(fill_lab, palette="primary")


  }

  if(var_x == "date"){
    q <- q + scale_x_date(name = lab_x)
  } else{
    q <- q + scale_x_discrete(name = lab_x, breaks = breaks_x)
  }

  if(!is.null(facet_wrap)){
    q <- q + lemon::facet_rep_wrap(~get(facet_wrap), repeat.tick.labels = "y", ncol = facet_ncol)

  }

  q <- q + scale_y_continuous(name = lab_y,
                              expand = expansion(mult = c(0, 0.1)),
                              breaks = splstyle::pretty_breaks(5),
                              labels = format_y
  )
  # q <- q + labs(caption = fhi_caption())
  q <- q + labs(title = lab_main,
                subtitle = lab_sub,
                caption = lab_caption,
                )
  q <- q + splstyle::theme_fhi_lines_horizontal()
  q <- q + splstyle::set_x_axis_vertical()
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
  # print(d)

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
  # print(d)

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

  d <- d[location_code %in% c(
    "county03",
    "county11",
    "county15"
    # "county30",
    # "county34"
  )]

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
