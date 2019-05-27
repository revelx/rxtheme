### COLORS ###

# set colors
  rx_colors <-  c(`yellow`     = "#FFCB05",
                `grey`         = "#939598",
                `light yellow` = "#F7D57B",
                `black`        = "#000000",
                `dark yellow`  = "#DAAD3E",
                `light grey`   = "#D9D9D9")

# internal function to return colors
  rx_cols <- function(...) {

    cols <- c(...)

    if (is.null(cols)) return (rx_colors)

    rx_colors[cols]
  }

# set palletes
  rx_palettes <- list( `main`  = rx_cols("yellow", "grey","black","dark yellow","light grey","light yellow"),
                       `marc`  = rx_cols("light yellow", "yellow","light grey")
  )





# internal function define pallette function
  rx_pal <- function(palette = "main",
                     reverse = FALSE, ...) {

    pal <- rx_palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal, ...)
  }


#' Scale color ggplot object
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
  scale_color_rx <- function( palette = "main",
                            discrete = TRUE,
                            reverse = FALSE, ...) {
  # assign pallette
    pal <- rx_pal(palette = palette, reverse = reverse)

  # check for discrete or continuous colors
    if (discrete) { discrete_scale("colour", paste0("rx_", palette), palette = pal, ...) }

    else { scale_color_gradientn(colours = pal(256), ...) }
}


#' Scale fill color for ggplot object
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @return
#' @export
  scale_fill_rx <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  # assing pallette
    pal <- rx_pal(palette = palette, reverse = reverse)

  # check for discrete or continous colors
    if (discrete) {  discrete_scale("fill", paste0("rx_", palette), palette = pal, ...) }

    else { scale_fill_gradientn(colours = pal(256), ...) }
}
