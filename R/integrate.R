#' Trapezoidal integration over range of x
#' @param x A numeric vector
#' @param y A numeric vector
#' @param cumulative Should the cumulative vector be returned? Default is \code{FALSE}
#' @param reverse Should integration start from the end? Default is \code{FALSE}
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
integrate <- function(x, y, cumulative = FALSE, reverse = FALSE) {
  ox <- order(x, decreasing = reverse)

  if (reverse == FALSE) {
    xs <- x
    ys <- y
  } else {
    xs <- rev(x)
    ys <- rev(y)
  }

  if (isTRUE(cumulative)) {
    surface <- c(0, cumsum((xs[-1] - xs[-length(xs)]) * (ys[-1] + ys[-length(ys)])) / 2)
  } else {
    surface <- sum((xs[-1] - xs[-length(xs)]) * (ys[-1] + ys[-length(ys)])) / 2
  }

  if (reverse == TRUE) surface <- -rev(surface)

  surface
}
