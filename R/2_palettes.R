#' Main colour extractor
#' https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' @param palette as
#' @param direction as
#' @export
fhi_pal <- function(palette = "primary", direction = 1) {
  if (!palette %in% vals$palettes) stop("Palette '{palette}' not in: ", paste0(vals$palettes, collapse = ", "))

  function(n) {
    pal_names <- stringr::str_subset(names(vals$pals), glue::glue("^{palette}_[0-9]+$"))
    nums_available <- stringr::str_remove(pal_names, paste0(palette, "_"))
    if (!n %in% nums_available) stop(glue::glue("Only {paste0(nums_available, collapse=', ')} levels allowed for {palette}"))

    pal <- vals$pals[[glue::glue("{palette}_{n}")]]
    if (direction == -1) {
      pal <- rev(pal)
    }

    retval <- pal[1:n]
    names(retval) <- NULL

    return(retval)
  }
}

#' Main colour extractor
#' https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' @param palette as
#' @param direction as
#' @param ... as
#' @export
scale_color_fhi <- scale_colour_fhi <- function(..., palette = "primary", direction = 1) {
  pal <- fhi_pal(palette = palette, direction = direction)

  ggplot2::discrete_scale("colour", paste0("fhi_", palette), palette = pal, ...)
}

#' Main colour extractor
#' https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' @param palette a
#' @param direction a
#' @param ... a
#' @export
scale_fill_fhi <- function(..., palette = "primary", direction = 1) {
  pal <- fhi_pal(palette = palette, direction = direction)

  ggplot2::discrete_scale("fill", paste0("fhi_", palette), palette = pal, ...)
}

#' Main colour extractor
#' https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' @import data.table ggplot2
#' @export
display_all_palettes <- function() {
  V2 <- NULL
  V3 <- NULL
  pal <- NULL
  x <- NULL

  tags <- vals$palettes
  to_plot <- vector("list", length = length(tags))

  for (i in seq_along(tags)) {
    p <- stringr::str_subset(rev(names(vals$pals)), glue::glue("^{tags[i]}_[0-9]+$"))[1]
    to_plot[[i]] <- data.table(pal = stringr::str_remove(p, "_[0-9]+$"), vals$pals[[p]], names(vals$pals[[p]]))
    to_plot[[i]][, x := 1:.N]
  }
  to_plot <- rbindlist(to_plot)

  cols <- unique(to_plot$V2)
  cols <- unique(cols)
  names(cols) <- cols

  to_plot[, pal := factor(pal, levels = tags)]

  q <- ggplot(to_plot, aes(x = x, y = pal, fill = V2, label = V3))
  q <- q + geom_tile(color = "black", height = 0.5, size = 1)
  q <- q + geom_text()
  q <- q + scale_fill_manual(values = cols)
  q <- q + scale_x_continuous("Level")
  q <- q + scale_y_discrete("Palette")
  q <- q + theme_fhi_basic()
  q <- q + theme(legend.position = "none")
  q
}

Display_All_Palettes <- function() {
  .Deprecated("display_all_palettes")
  display_all_palettes()
}
