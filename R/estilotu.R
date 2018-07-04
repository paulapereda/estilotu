#' @title Tema de Transforma Uruguay.
#'
#' @description Configura como default el tema de Transforma Uruguay para los gráficos creados en ggplot2.
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export
#'
#' @import sysfonts, ggthemr



sysfonts::font_add("MyriadPro", "MyriadPro-Regular.otf")


paleta_transforma <- function() {

  ggthemr::define_palette(
    swatch = c(
      "#BACB33",
      "#F7941E",
      "#999B9E",
      "#ffcc00",
      "#2974B4",
      "#538fc3",
      "#d0400a",
      "#0a98cc",
      "#94a228",
      "#ffe066",
      "#865eae"),
    gradient = c(lower = "#ffcc00", upper = "#BACB33")
  )

}

tema_transforma <- function(base_size = 12) {

  theme_gray(base_size, base_family = "MyriadPro") +
    theme(panel.grid.major = element_line(color = "#b6b6b6"),
          plot.background  = element_rect(fill="#ffffff"),
          panel.background = element_rect(fill="#ffffff"),
          panel.grid.minor = element_line("#D5D0CE"),
          axis.ticks =  element_line(colour = "#b6b6b6"),
          text = element_text(family = "MyriadPro"))

}

fuente_transforma <- function() {

  update_geom_defaults("text", list(family = "MyriadPro"))
  update_geom_defaults("label", list(family = "MyriadPro"))

}

set_estilotu <- function(base_size = 12)  {

  ggthemr::ggthemr(paleta_transforma())

  theme_set(tema_transforma(base_size = base_size))

  fuente_transforma()

}

reset_estilotu <- function() {

  ggthemr::ggthemr_reset()

}
