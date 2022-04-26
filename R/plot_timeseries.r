
 # data <- test_data_time_series()
 # plot_timeseries(data, var_y = c("cases_n", "deaths_n"))
#  plot_timeseries(data,
#                   var_y = c("Covid cases" = "cases_n", "Covid deaths" ="deaths_n", "Covid tests" = "tests_n"),
#                   breaks_x = splstyle::every_nth(2),
#                   lab_main = "Norge",
#                   lab_sub = "Antall tilfeller og dødsfall",
#                  lab_y = "Antall",
#                  lab_x = "Uker",
#                  lab_legend = "Legend",
#                  palette = "warning"
#
# )


#  plot_timeseries(data,
#                   var_y = c("Covid cases" = "cases_n", "Covid deaths" ="deaths_n"),
#                   breaks_x = splstyle::every_nth(2),
#                   lab_main = "Norge",
#                   lab_sub = "Antall tilfeller og dødsfall",
#                  lab_y = "Antall",
#                  lab_x = "Uker",
#                  lab_legend = "Legend",
#                  facet_wrap = "location_code",
#                  facet_ncol = 4
#
# )


plot_timeseries <- function(data,
                            var_x = "isoyearweek",
                            var_y,
                            breaks_x = NULL,
                            lab_main = NULL,
                            lab_sub = NULL,
                            lab_caption = splstyle::fhi_caption(),
                            lab_y = NULL,
                            lab_x = NULL,
                            lab_legend = NULL,
                            legend_position = "bottom",
                            format_y = splstyle::format_nor_num_0,
                            facet_wrap = NULL,
                            facet_ncol = NULL,
                            palette = "primary",
                            scale_y = "free"
                            ) {



  d <- melt(data,
              id.vars = c(facet_wrap, "isoyearweek"),
              measure.vars = list(n = var_y),
              value.name = "n"
  )

  d_name <- data.table(name_outcome= names(var_y), variable = var_y)

  # d <- cbind(d, d_name)
  d <- d_name[d, on = 'variable']


  q <- ggplot(d, aes_string(x = var_x))
  q <- q + geom_path(aes(y = n, color = name_outcome, group = name_outcome), size = 1.5)
  q <- q + scale_x_discrete(name = lab_x, breaks = breaks_x)
  q <- q + scale_y_continuous(name = lab_y,
                              breaks = fhiplot::pretty_breaks(10),
                              expand = expand_scale(mult = c(0, 0.1)),
                              labels = format_y
                              )
  if(!is.null(facet_wrap)){
    q <- q + lemon::facet_rep_wrap(~get(facet_wrap), repeat.tick.labels = "y", scales = scale_y, ncol = facet_ncol)

  }



  q <- q + expand_limits(y = 0)
  q <- q + fhiplot::scale_color_fhi(lab_legend, palette = palette, direction = 1)
  # q <- q + guides(color = guide_legend(order = 1, reverse = F), color = guide_legend(order = 2))
  q <- q + splstyle::theme_fhi_lines_horizontal(legend_position = legend_position)
  # q <- q + fhiplot::theme_fhi_basic(base_size = 9, legend_position = "bottom")
  q <- q + labs(title = lab_main,
                subtitle = lab_sub,
                caption = lab_caption
                )
  q <- q + fhiplot::set_x_axis_vertical()
  q

}



test_data_time_series <- function(var_x = "isoyearweek") {
  set.seed(4)
  dates <- sample(seq.Date(as.Date("2018-01-01"),
                           as.Date("2018-07-08"), 1),
                  20000,
                  replace = T)
  d <- expand.grid(
    location_code = "norge",
    # location_code = unique(fhidata::norway_locations_b2020$county_code),
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
           cases_n = .N
         ),
         keyby = .(
           location_code,
           date
         )
  ]

  d[, deaths_n := cases_n]
  d[, tests_n := cases_n]
  # aggregated daily dataset that does not contain days with 0 cases
  # print(d)

  # create skeleton
  skeleton <- data.table(expand.grid(
    location_code = "norge",
    # location_code = unique(fhidata::norway_locations_b2020$county_code),
    date = seq.Date(min(d$date), max(d$date), 1)
  ))

  # merge the two datasets together
  d <- merge(d, skeleton, by=c("location_code", "date"), all=T)

  # Fill in 'missing' Ns with 0
  d[is.na(cases_n), cases_n := 0]

  # Now you have a clean aggregated daily dataset that contains days with 0 cases!
  # print(d)

  # d <- d[location_code %in% c(
  #   "county03",
  #   "county11",
  #   "county15"
  #   # "county30",
  #   # "county34"
  # )]

  if(!is.null(var_x)){
    # create 3 new variables:
    d[, isoyearweek := fhi::isoyearweek(date)]

    # aggregate down to weekly level
    w <- d[,
           .(
             cases_n = sum(cases_n),
             deaths_n = sum(deaths_n),
             tests_n = sum(tests_n)
           ),
           keyby = .(
             location_code,
             isoyearweek
           )
    ]

    w[, deaths_n := deaths_n - 100]
    w[, tests_n := tests_n - 200]
    w[, cases_n := as.numeric(cases_n)]
    print(w)
    return(w)
  }

  return(d)
}
