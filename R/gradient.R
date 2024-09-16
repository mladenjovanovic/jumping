#' Simple gradient over range of x
#'
#' @description
#' Finds gradient of interpolated \code{y} over \code{x}. By default,
#'     function returns gradient of \code{y} for all \code{x} elements.
#'     If gradient at specific \code{x} value is needed, use \code{x_value}
#'     parameter. Please see examples for more information
#'
#' @param x A numeric vector
#' @param y A numeric vector
#' @param x_value Numeric vector of \code{x} values for which gradient
#'      is calculated. Default is equal to all \code{x} parameter values
#' @param ... Forwarded to the \link[numDeriv]{grad} function
#' @examples
#' df <- dplyr::tibble(
#'   x = seq(0, 2 * pi, length = 100),
#'   y = sin(x) + 0.75 * sin(3 * x) + 0.45 * sin(5 * x) + 0.25 * sin(7 * x),
#'   grad = gradient(x, y)
#' )
#'
#' df %>%
#'   ggplot2::ggplot(ggplot2::aes(x = x)) +
#'   ggplot2::geom_point(ggplot2::aes(y = y)) +
#'   ggplot2::geom_line(ggplot2::aes(y = grad), color = "red")
#'
#' # Find a gradient for a specific value of x
#' gradient(df$x, df$y, x_value = 2)
#'
#' @export
gradient <- function(x, y, x_value = x, ...) {
  f <- stats::approxfun(x, y)

  numDeriv::grad(func = f, x = x_value, method = "simple", ...)
}
