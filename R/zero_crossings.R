#' Find zero crossings of a vector
#' @description
#' Find zero crossings of a vector, i.e., indices when the numeric variable crosses 0.
#'     If \code{y} parameter is not provided (default), then the function returns
#'     interpolated indices where \code{x} vector crosses 0. If \code{y} vector is
#'     provided, then the function returns interpolated indices of \code{x} vector when
#'      \code{y} vector crosses 0.
#'
#'
#' @param x A numeric vector
#' @param y A numeric vector. Default is \code{NULL}
#' @returns A numeric vector or interpolated locations of zero-crossings in either \code{x}
#'     or \code{y}. See description and examples for more information
#'
#' @examples
#' df <- dplyr::tibble(
#'   t = seq(0, pi, length = 100),
#'   f = sin(t) + 0.5 * sin(3 * t) + 0.45 * sin(5 * t) +  0.25 * sin(7 * t) - 0.65,
#' )
#'
#' gg <- df %>%
#'   ggplot2::ggplot() +
#'   ggplot2::geom_hline(yintercept = 0) +
#'   ggplot2::geom_point(ggplot2::aes(x = t, y = f))
#'
#' gg
#'
#' # Where does the y crosses the 0? Returns interpolated location using x
#' cross0 <- zero_crossings(x = df$t, y = df$f)
#'
#' gg +
#'   ggplot2::geom_vline(xintercept = cross0, color = "red")
#'
#' # When using only x, function returns interpolated location in x
#' # This is because the actual x observation might not be located on 0
#' zero_crossings(x = df$f)
#'
#' # If you want to pull the closes points, make sure to round the indices
#' # to closes integer (using either round, floor, or ceiling functions)
#'
#' cross0 <- zero_crossings(x = df$f)
#'
#' floor_i <- floor(cross0)
#' ceiling_i <- ceiling(cross0)
#'
#' closest_obs <- c(floor_i, ceiling_i)
#'
#' gg +
#'   ggplot2::geom_point(
#'     data = df[closest_obs, ],
#'     ggplot2::aes(x = t, y = f),
#'     color = "red"
#'   )
#'
#' @export
zero_crossings <- function(x, y = NULL) {

  if (is.null(y)) {
    zerocrossings <- rootSolve::uniroot.all(
      stats::approxfun(seq_len(length(x)), x),
      interval = range(seq_len(length(x))))
  } else {
    zerocrossings <- rootSolve::uniroot.all(
      stats::approxfun(x, y),
      interval = range(x))
  }

  if (length(zerocrossings) == 0) {
    return(NA)
  }

  zerocrossings
}
