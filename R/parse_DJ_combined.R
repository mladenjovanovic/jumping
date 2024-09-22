parse_DJ_combined <- function(time,
                                 force,
                                 mass,
                                 gravity_const = 9.80665,
                                 contact_threshold = 20,
                                 na.rm = FALSE) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  velocity_height_method <- NULL
  velocity_ft_method <- NULL
  velocity <- NULL
  height_from_start <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  same_height_method <- parse_DJ_same_landing_height(
    time = time,
    force = force,
    mass = mass,
    gravity_const = gravity_const,
    contact_threshold = contact_threshold,
    na.rm = na.rm)

  flight_time_method <- parse_DJ_flight_time(
    time = time,
    force = force,
    mass = mass,
    gravity_const = gravity_const,
    contact_threshold = contact_threshold,
    na.rm = na.rm)

  # Combine the two
  trace <- same_height_method$trace %>%
    dplyr::mutate(
      velocity_height_method = same_height_method$trace$velocity,
      velocity_ft_method =  flight_time_method$trace$velocity,
      velocity = (velocity_height_method + velocity_ft_method)/2
    )

  # ==================================
  # Integrate
  trace <- trace %>%
    dplyr::mutate(
      height_from_start = integrate(time, velocity, cumulative = TRUE)
    )

  # Find the height at start of the movement
  start_time_height <- trace$height_from_start[trace$time >= same_height_method$movement_start_time][1]

  # That height should be zero
  trace$height_from_start <- trace$height_from_start - start_time_height

  # Create height from take off origin variable
  take_off_height <- trace$height_from_start[trace$time >= same_height_method$take_off_time][1]
  trace <- trace %>%
    dplyr::mutate(
      height_from_take_off = height_from_start - take_off_height
    )

  # Return list
  list(
    trace = trace,

    movement_start_time = same_height_method$movement_start_time,
    take_off_time = same_height_method$take_off_time,
    landing_time = same_height_method$landing_time
  )

}
