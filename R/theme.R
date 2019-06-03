#' Function to apply RevelX Theme to a ggplot object
#'
#' @param base_size
#' Set the base size, default is 12
#' @param base_family
#' Set Font familiy , default is Lato
#' @param grid_lines
#' Set gridline color, default is NA. Color can be a valid hex value.
#' @param background_color
#' Set background color, default is white. Color can be a valid hex value.
#' @param ...
#'
#' @return
#'
theme_rx <- function ( base_size = 12,
                       base_family = "Lato",
                       grid_lines = NA,
                       background_color = "white",
                       ...)
{

  # set half line
  half_line <- (base_size / 2)


  # set background


  theme(
    line = element_line( colour = "black",
                         size = 0.5,
                         linetype = 1,
                         lineend = "butt"),
    rect = element_rect( fill = "white",
                         colour = "black",
                         size = 0.5,
                         linetype = 1),
    text = element_text( family = base_family,
                         face = "plain",
                         colour = "black",
                         size = base_size,
                         lineheight = 0.9,
                         hjust = 0.5,
                         vjust = 0.5,
                         angle = 0,
                         margin = margin(),
                         debug = FALSE),
    axis.line = element_line(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(0.8),
                             colour = "black"),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.length = unit(half_line/2, "pt"),
    axis.title.x = element_text(margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2)),
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
    legend.background = element_rect(colour = background_color, fill = background_color),
    legend.margin = margin(0.2),
    legend.key = element_rect(fill = background_color, colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.spacing.x = unit(0.2, 'cm'),
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = background_color , colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = grid_lines),
    panel.grid.minor = element_line(colour = grid_lines, size = 0.25),
    panel.spacing = unit(half_line, "pt"), panel.margin.x = NULL,
    panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "#eeeeee", colour = "grey85"),
    strip.text = element_text(colour = "black", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect( colour = background_color, fill = background_color),
    plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)))
}
