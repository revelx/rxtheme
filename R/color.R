### COLORS ###

# set colors
  rx_colors <-  c(`yellow`                = "#FFCB05",
                  `grey`                  = "#939598",
                  `light yellow`          = "#F7D57B",
                  `black`                 = "#000000",
                  `dark yellow`           = "#DAAD3E",
                  `light grey`            = "#D9D9D9",
                  `dark puce`             = "#433A3F",
                  `deep space sparkle`    = "#3D5A6C",
                  `green sheen`           = "#72A98F",
                  `kiwi`                  = "#8DE969",
                  `maximum green yellow` = "#CBEF43" ,
                  `nm_blue` ="#01009a",
                  `nm_green` = "#007866",
                  `nm_green_light` = "#89b934",
                  `nm_pink` = "#e2006a",
                  `nm_blue_light` = "#00aec7"
                  )

# internal function to return colors
  rx_cols <- function(...) {

    cols <- c(...)

    if (is.null(cols)) return (rx_colors)

    rx_colors[cols]
  }

# set palletes
  ## green pallete from https://coolors.co/433a3f-3d5a6c-72a98f-8de969-cbef43
  rx_palettes <- list( `main`                = rx_cols( "yellow", "black","dark yellow","light grey","light yellow","grey" ),
                       `marc`                = rx_cols( "light yellow", "yellow","light grey"),
                       `green`               = rx_cols ("dark puce", "deep space sparkle","green sheen","kiwi","maximum green yellow"),
                       `natuurmonumenten`    = rx_cols("nm_blue","nm_green" ,"nm_green_light","nm_pink","nm_blue_light")
  )


# internal function define pallette function
  rx_pal <- function(palette = "main",
                     reverse = FALSE, ...) {

    pal <- rx_palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal, ...)
  }


#' Show available palettes
#'
#' @return
#' Names list with palette names , color names and hex codes
#' @export
  show_palette <- function(){
    return(rx_palettes)
  }

#' Scale color ggplot object
#'
#' @param palette
#' select which pallete
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
