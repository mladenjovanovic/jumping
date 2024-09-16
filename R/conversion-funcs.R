#' Get Height from TOV
#'
#' Function \code{height_from_TOV} calculates height reached using the ballistic equation
#' @param take_off_velocity Numeric vector in m/s
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' height_from_TOV(2.75)
height_from_TOV <- function(take_off_velocity, gravity_const = 9.81) {
  (take_off_velocity^2) / (2 * gravity_const)
}

#' Get Height from Flight Time
#'
#' \code{height_from_FT} returns jump height using \code{flight_time}
#' @param flight_time Numeric vector. Flight time in seconds
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector. Jump height in meters
#' @export
#' @examples
#' height_from_FT(0.7)
height_from_FT <- function(flight_time, gravity_const = 9.81) {
  1 / 8 * gravity_const * flight_time^2
}

#' Get Take-off Velocity from Flight Time
#'
#' \code{height_from_FT} returns take-off velocity using \code{flight_time}
#' @param flight_time Numeric vector. Flight time in seconds
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector. Take-off velocity in m/s
#' @export
#' @examples
#' TOV_from_FT(0.75)
TOV_from_FT <- function(flight_time, gravity_const = 9.81) {
  1 / 2 * gravity_const * flight_time
}

#' Get Flight Time from Take-off Velocity
#'
#' \code{FT_from_TOV} returns flight time using \code{take_off_velocity}
#' @param take_off_velocity Numeric vector in m/s
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector. Flight time in seconds
#' @export
#' @examples
#' height_from_FT(0.7)
FT_from_TOV <- function(take_off_velocity, gravity_const = 9.81) {
  2 * take_off_velocity / gravity_const
}

#' Get Take-off Velocity from Height
#'
#' \code{TOV_from_height} returns take-off velocity using \code{height}
#' @param height Numeric vector in meters
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector. Flight time in seconds
#' @export
#' @examples
#' TOV_from_height(0.3)
TOV_from_height <- function(height, gravity_const = 9.81) {
  sqrt(2 * height * gravity_const)
}

#' Get Flight Time from Height
#'
#' \code{FT_from_height} returns flight time using \code{height}
#' @param height Numeric vector in meters
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector. Flight time in seconds
#' @export
#' @examples
#' FT_from_height(0.3)
FT_from_height <- function(height, gravity_const = 9.81) {
  sqrt(8 * height / gravity_const)
}
