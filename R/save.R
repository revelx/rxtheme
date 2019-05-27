
#' Function for saving ggplot objects to PowerPoint ready images
#'
#' @param ggplot_object
#' defaults to the last shown image
#' @param file_name
#' file name including folder and extension. Png is prefferred.
#' @param type
#' wide for full width of the slide, semi for half of the slide, three for three quarters of the slide
#' @param dpi
#' print, screen, retina or a dpi setting. 300 is usually fine.
#' @param ...
#'
#' @return
#' @export
save_rx <- function( ggplot_object,
                     file_name =" export.png",
                     type = "wide",
                     dpi = "print",
                     ...) {
#

# set width dimension
  width_rx <-   case_when(
                          type == "wide"  ~  29.87,
                          type == "semi"  ~  14.33,
                          type == "three" ~  19.58,
                          TRUE ~  29.87
                        )

# set height dimension
  height_rx <-   case_when(
                          type == "wide"  ~  12.14,
                          type == "semi"  ~  9.86,
                          type == "three" ~  11.79,
                          TRUE ~  29.87
                        )


# call ggsave from ggplot
  ggsave(  file_name,
           width = width_rx ,
           height = height_rx,
           units = "cm",
           dpi = dpi)



}
