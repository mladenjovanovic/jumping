parse_force_trace <- function(time,
                             force,
                             mass,
                             gravity_const = 9.80665,
                             threshold_type = c("both", "upper", "lower"),
                             start_threshold = 20,
                             start_velocity = 0,
                             start_height = 0,
                             na.rm = FALSE) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  acceleration <- NULL
  force_net <- NULL
  height_from_start <- NULL
  power <- NULL
  velocity <- NULL
  weight <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!(threshold_type[1] %in% c("both", "upper", "lower"))) {
    stop("Invalid threshold type. Please use `both`, `upper`, or `lower`")
  }

  trace <- data.frame(
    time = time,
    force = force,
    mass = mass,
    gravity_const = gravity_const
  )

  if (na.rm == TRUE) trace <- trace %>% stats::na.omit()


  # Find the start of integration
  if (!is.null(start_threshold)) {
    # Find thresholds
    upper_start_threshold <- mass * gravity_const + start_threshold
    lower_start_threshold <- mass * gravity_const - start_threshold

    # Find first threshold crossing
    if (threshold_type[1] == "both") {
      movement_start_index <- which(trace$force > upper_start_threshold | trace$force < lower_start_threshold)[1]
    } else if (threshold_type[1] == "upper") {
      movement_start_index <- which(trace$force > upper_start_threshold)[1]
    } else if (threshold_type[1] == "lower") {
      movement_start_index <- which(trace$force < lower_start_threshold)[1]
    }

    movement_start_time <- trace$time[movement_start_index]
  } else {
    # just use start of the trace
    movement_start_time <- trace$time[1]
  }

  # Integrate
  trace <- trace %>%
    dplyr::mutate(
      weight = mass * gravity_const,
      time_from_start = time - movement_start_time,
      force_net = force - weight,
      acceleration = force_net / mass,
      # Make acceleration before movement start time equal to 0
      acceleration = dplyr::if_else(time < movement_start_time, 0, acceleration),
      velocity = start_velocity + integrate(time, acceleration, cumulative = TRUE),
      height_from_start = start_height + integrate(time, velocity, cumulative = TRUE),
      impulse = integrate(time, force, cumulative = TRUE),
      impulse_net = integrate(time, force_net, cumulative = TRUE),
      power = force * velocity,
      work = integrate(time, power, cumulative = TRUE)
    )

  trace


}
