#' Trapezoidal integration over range of x
#' @param x A numeric vector
#' @param y A numeric vector
#' @param cumulative Should the cumulative vector be returned? Default is \code{FALSE}
#' @returns Numeric vector
#' @examples
#' df <- dplyr::tibble(
#'   x = seq(0, 2 * pi, length = 100),
#'   y = sin(x),
#'   j = integrate(x, y, cumulative = TRUE)
#' )
#'
#' df %>%
#'   ggplot2::ggplot(ggplot2::aes(x = x)) +
#'   ggplot2::geom_point(ggplot2::aes(y = y)) +
#'   ggplot2::geom_point(ggplot2::aes(y = j), color = "red")
#'
#' # Total surface
#' integrate(df$x, df$y)
#'
#' @export
integrate <- function(x, y, cumulative = FALSE) {
  ox = order(x)
  xs = x[ox]
  ys = y[ox]

  if (isTRUE(cumulative)) {
    c(0, cumsum((xs[-1] - xs[-length(xs)]) * (ys[-1] + ys[-length(ys)]))/2)
  } else {
    sum((xs[-1] - xs[-length(xs)]) * (ys[-1] + ys[-length(ys)]))/2
  }

}
