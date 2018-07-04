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
#' @import sysfonts



sysfonts::font_add("MyriadPro", "MyriadPro-Regular.otf")

#' @export
is_colour <- function(x)
  UseMethod('is_colour', x)


#' @rdname is_colour
#' @export
#' @importFrom grDevices colors
is_colour.character <- function(x)
  grepl('#[a-f0-9]{6}', x, TRUE) | x %in% colors() | is.na(x)


#' @rdname is_colour
#' @export
is_colour.numeric <- function(x)
  x %in% seq_along(grDevices::palette())


#' @rdname is_colour
#' @export
is_colour.logical <- function(x)
  is.na(x)

#' @rdname is_colour
#' @export
is_colour.factor <- function(x)
  is_colour.character(as.character(x))

validate_colours <- function(x, on_invalid = stop) {
  validation <- is_colour(x)
  n_invalid <- sum(!validation)
  if (n_invalid == 1) {
    on_invalid(x[!validation], ' is not a valid colour.', call. = FALSE)
  } else if (n_invalid > 1) {
    on_invalid(display_list(x[!validation]), ' are not valid colours.', call. = FALSE)
  }
}

define_swatch <- function(x) {
  n <- length(x)
  if (n < 1)
    stop('No colours provided for swatch.', call. = FALSE)
  if (n < 2)
    stop('You must have at least two colours in the swatch (seven recommended).', call. = FALSE)
  validate_colours(x)
  if (n < 6)
    warning('It is recommended that you provide at least seven colours for the swatch.', call. = FALSE)
  if (any(duplicated(sapply(x, unname_colour))))
    stop('Duplicate colours in swatch.', call. = FALSE)
  structure(x, class = 'ggthemr_swatch')
}

#' @export
#' @rdname define_swatch

ggthemr_swatch <- define_swatch

define_palette <- function(swatch, gradient,
                           background = '#ffffff',
                           text = c('#444444', '#444444'),
                           line = c('#6e6e6e', '#6e6e6e'),
                           gridline = '#c3c3c3'
) {

  n_text <- length(text)
  if (n_text == 1L) {
    warning('You should supply a vector of length two for the text colour. The first element being the colour used for plots of type "inner". The second when for plots of type "outer".', call. = FALSE)
    text_inner <- text
    text_outer <- text
  } else if (n_text > 2L) {
    warning('You have supplied two many colours for the text colour. Only the first two will be used.', call. = FALSE)
  } else {
    text_inner <- text[1L]
    text_outer <- text[2L]
  }

  n_line <- length(line)
  if (n_line == 1L) {
    warning('You should supply a vector of length two for the axis line colour. The first element being the colour used for plots of type "inner". The second when for plots of type "outer".', call. = FALSE)
    line_inner <- line
    line_outer <- line
  } else if (n_line > 2L) {
    warning('You have supplied two many colours for the axes line colour. Only the first two will be used.', call. = FALSE)
  } else {
    line_inner <- line[1L]
    line_outer <- line[2L]
  }

  if (any(duplicated(sapply(swatch, unname_colour)))) warning('Duplicate plot colours.')
  clash_warning_any('Some plot colours are the same as the background.', swatch, background)
  clash_warning_any('Some gradient colours are the same as the background.', gradient, background)

  clash_error("The inner text colour is the same as the background colour. Text will not be visible.", background, text_inner)
  clash_error("The outer text colour is the same as the background colour. Text will not be visible.", background, text_outer)

  clash_warning("The inner line colour is the same as the background colour. Axes lines will not be visible. If this is desired it is more appropriate to a layout where axes lines are not present.", line_inner, background)
  clash_warning("The outer line colour is the same as the background colour. Axes lines will not be visible. If this is desired it is more appropriate to a layout where axes lines are not present.", line_outer, background)

  clash_warning("The gridline colour is the same as the background colour. Gridlines will not be visible. If this is desired it is more appropriate to define a new layout where gridlines are not present.", gridline, background)

  palette <- list(
    background = background,
    text = c(inner = text_inner, outer = text_outer),
    line = c(inner = line_inner, outer = line_outer),
    gridline = gridline,
    swatch = define_swatch(swatch),
    gradient = define_gradient(gradient)
  )

  if (!all(sapply(unlist(palette), is.character)))
    stop('You must only supply colours as characters.', call. = FALSE)

  class(palette) <- 'ggthemr_palette'

  return (palette)

}

#' @rdname define_palette
#' @export
ggthemr_palette <- define_palette

colour_clash <- function(...) {
  x <- c(...)
  named_colours <- !grepl('#[a-z0-9]{6}', x, TRUE)
  x[named_colours] <- sapply(x[named_colours], unname_colour)
  any(duplicated(tolower(x)))
}

clash_warning_any <- function(warning_message, m_colour, s_colour) {
  if (any(sapply(m_colour, colour_clash, s_colour)))
    warning(warning_message, call. = FALSE)
}

#' @importFrom grDevices rgb col2rgb
unname_colour <- function(x)
  do.call(rgb, as.list(col2rgb(x) / 255))

clash_warning <- function(warning_message, ...)
  if (colour_clash(...)) warning(warning_message, call. = FALSE)

clash_error <- function(stop_message, ...)
  if (colour_clash(...)) stop(stop_message, call. = FALSE)

define_gradient <- function (x) {

  if (length(x) < 2L)
    stop("You must provide two gradient colours (low and high respectively).", call. = FALSE)

  if (length(x) > 2L)
    warning('You have supplied too many colours for the gradient. Taking only the first two or those named "low" and "high".', call. = FALSE)

  if (x[[1L]] == x[[2L]])
    stop("The gradient colours are the same.", call. = FALSE)

  nm <- names(x)
  if (length(nm) == 2L & all(c('low', 'high') %in% nm)) {
    return (c(low = x[['low']], high = x[['high']]))
  } else {
    return (c(low = x[[1L]], high = x[[2L]]))
  }

}

get_theme_palette <- function() {

  define_palette(
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

theme_our <- function(base_size = 12) {

  theme_gray(base_size, base_family = "MyriadPro") +
  theme(panel.grid.major = element_line(color = "#b6b6b6"),
          plot.background  = element_rect(fill="#ffffff"),
          panel.background = element_rect(fill="#ffffff"),
          panel.grid.minor = element_line("#D5D0CE"),
          axis.ticks =  element_line(colour = "#b6b6b6"),
          text = element_text(family = "MyriadPro"))

}

update_font_defaults <- function() {

  update_geom_defaults("text", list(family = "MyriadPro"))
  update_geom_defaults("label", list(family = "MyriadPro"))

}

set_estilotu <- function(base_size = 12)  {

  ggthemr(get_theme_palette())

  theme_set(theme_our(base_size = base_size))

  update_font_defaults()

}
