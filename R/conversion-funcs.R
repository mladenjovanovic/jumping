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
#' @return Numeric vector in Newtons
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
#' @return Numeric vector in Newtons
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
#' @param force Numeric vector in Newtons
#' @param mass Numeric vector in kilograms
#' @param push_off_distance Numeric vector in meters. Default 0.4 meters
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector in m/s
#' @export
#' @examples
#' TOV_from_force(
#'   force = 2000,
#'   mass = 85,
#'   push_off_distance = 0.4
#' )
TOV_from_force <- function(force,
                           mass,
                           push_off_distance = 0.4,
                           gravity_const = 9.81) {
  sqrt(2 * push_off_distance * (force / mass - gravity_const))
}


#' Get Take-Off Velocity from Force-Velocity Profile
#'
#' \code{TOV_from_FV_profile} returns predicted maximal take off velocity that could be achieve based on the Samozino \emph{et al.}
#'     model which uses vertical jump profile \code{F0} and \code{V0}.
#' @param F0 Numeric vector in Newtons
#' @param V0 Numeric vector in m/s
#' @param mass Numeric vector in kilograms
#' @param push_off_distance Numeric vector in meters. Default 0.4 meters
#' @param gravity_const Numeric vector. Default 9.81
#' @return Numeric vector representing maximal take-off velocity in m/s
#' @export
#' @references
#'     Samozino, Pierre. ‘A Simple Method for Measuring Lower Limb Force, Velocity and Power Capabilities During Jumping’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 65–96. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_4.
#'
#'     ———. ‘Optimal Force-Velocity Profile in Ballistic Push-off: Measurement and Relationship with Performance’. In Biomechanics of Training and Testing, edited by Jean-Benoit Morin and Pierre Samozino, 97–119. Cham: Springer International Publishing, 2018. https://doi.org/10.1007/978-3-319-05633-3_5.
#'
#'     Samozino, Pierre, Jean-Benoît Morin, Frédérique Hintzy, and Alain Belli. ‘Jumping Ability: A Theoretical Integrative Approach’. Journal of Theoretical Biology 264, no. 1 (May 2010): 11–18. https://doi.org/10.1016/j.jtbi.2010.01.021.
#'
#'     Samozino, Pierre, Enrico Rejc, Pietro Enrico Di Prampero, Alain Belli, and Jean-Benoît Morin. ‘Optimal Force–Velocity Profile in Ballistic Movements—Altius’: Medicine & Science in Sports & Exercise 44, no. 2 (February 2012): 313–22. https://doi.org/10.1249/MSS.0b013e31822d757a.
#' @examples
#' TOV_from_FV_profile(F0 = 2500, V0 = 3.7, mass = 85, push_off_distance = 0.42)
#'
TOV_from_FV_profile <- function(F0,
                                V0,
                                mass,
                                push_off_distance = 0.4,
                                gravity_const = 9.81) {
  (push_off_distance * (sqrt(F0^2 / (4 * V0^2) - (2 * mass * (gravity_const * mass - F0)) / push_off_distance) - F0 / (2 * V0))) / mass
}
