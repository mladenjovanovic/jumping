# Get all streaks
get_streaks <- function(x) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  lagged_x <- NULL
  streak_start <- NULL
  streak_id <- NULL
  index <- NULL
  streak <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  df <- data.frame(
    index = seq_along(x),
    x = x
  )

  df <- df %>%
    dplyr::mutate(lagged_x = dplyr::lag(x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(streak_start = !identical(x, lagged_x)) %>% # (x != lagged_x))
    dplyr::ungroup()

  df[1, "streak_start"] <- TRUE

  df <- df %>%
    dplyr::mutate(streak_id = cumsum(streak_start))

  df <- df %>%
    dplyr::group_by(streak_id) %>%
    dplyr::mutate(streak = dplyr::row_number()) %>%
    dplyr::summarise(
      value = x[1],
      start = min(index),
      stop = max(index),
      length = max(streak)
    ) %>%
    dplyr::ungroup()

  df
}

# Get longest streaks per value
get_longest_streaks <- function(x) {
  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  value <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  get_streaks(x) %>%
    dplyr::group_by(value) %>%
    dplyr::slice_max(length) %>%
    dplyr::ungroup()
}

#' Longest \code{TRUE} streak
#'
#' @description
#' Finds the location (i.e., start and stop index) of the longest \code{TRUE} streak
#'     in the logical \code{x} vector
#'
#' @param x A logical vector
#' @returns A numeric two-element vector with \code{start} and \code{stop} indices of the
#'     longest \code{TRUE} value streak in \code{x}
#' @examples
#' longest_TRUE_streak(c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
#'
#' longest_TRUE_streak(1:10 > 3)
#'
#' x <- seq(1, pi, length.out = 100)
#' y <- sin(x)
#'
#' x[longest_TRUE_streak(y > 0.25 & y < 0.7)]
#'
#' @export
longest_TRUE_streak <- function(x) {
  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  value <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!is.logical(x)) {
    stop("'x' must be logical vector")
  }

  df <- get_longest_streaks(x)

  # Return the longest streak of the TRUE value
  df <- df %>%
    dplyr::filter(value == TRUE)

  res <- c(df$start, df$stop)
  names(res) <- c("start", "stop")
  res
}
