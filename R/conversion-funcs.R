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

#' Get Mean Force over Distance
#'
#' Function \code{force_over_distance} calculates mean force over push-off distance using work equation
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param push_off_distance Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' force_over_distance(100, 100 * 9.81, 4, 0.4)
force_over_distance <- function(mass,
                                weight = mass * 9.81,
                                take_off_velocity,
                                push_off_distance) {
  weight + (mass * (take_off_velocity^2) / (2 * push_off_distance))
}


#' Get Mean Force over Time
#'
#' Function \code{force_over_time} calculates mean force over push-off time using impulse equation
#' @param mass Numeric vector
#' @param weight Numeric vector. Default \code{mass} * 9.81
#' @param take_off_velocity Numeric vector
#' @param time_taken Numeric vector
#' @return Numeric vector
#' @export
#' @examples
#' force_over_time(100, 100 * 9.81, 4, 0.3)
force_over_time <- function(mass,
                            weight = mass * 9.81,
                            take_off_velocity,
                            time_taken) {
  (mass * take_off_velocity / (time_taken)) + (weight)
}

#' Get Take-off Velocity
#'
#' \code{TOV_from_force} returns take off velocity when \code{force} is applied to the object
#' of \code{mass} over \code{push_off_distance} in vertical direction, assuming zero starting velocity
#' @param force Numeric vector. Default 3000
#' @param mass Numeric vector. Default 75
#' @param push_off_distance Numeric vector. Default 0.4
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector
#' @export
#' @examples
#' TOV_from_force(
#'   force = 2000,
#'   mass = 85,
#'   push_off_distance = 0.4
#' )
TOV_from_force <- function(force = 3000,
                           mass = 75,
                           push_off_distance = 0.4,
                           gravity_const = 9.81) {
  sqrt(2 * push_off_distance * (force / mass - gravity_const))
}
