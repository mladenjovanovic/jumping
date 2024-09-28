parse_DJ_same_landing_height <- function(time,
                                         force,
                                         mass,
                                         gravity_const = 9.80665,
                                         contact_threshold = 20,
                                         na.rm = FALSE) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  weight <- NULL
  force_net <- NULL
  acceleration <- NULL
  acc_after_drop <- NULL
  velocity <- NULL
  dropping_velocity <- NULL
  height_from_start <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  trace <- data.frame(
    time = time,
    force = force,
    mass = mass,
    gravity_const
  )

  if (na.rm == TRUE) trace <- trace %>% stats::na.omit()

  # =============================================
  # Find start of DJ
  movement_start_index <- which(trace$force > contact_threshold)[1]
  movement_start_time <- trace$time[movement_start_index]

  # =================================
  # Find the flight phase
  # After drop phase
  after_drop_trace <- trace %>%
    dplyr::filter(time >= movement_start_time)

  # Find the longest streak of "zero" force
  flight_phase_index <- longest_TRUE_streak(after_drop_trace$force < contact_threshold)
  flight_phase_time <- after_drop_trace$time[flight_phase_index]

  take_off_index <- flight_phase_index[1]
  take_off_time <- after_drop_trace$time[take_off_index]

  landing_index <- flight_phase_index[2] + 1
  landing_time <- after_drop_trace$time[landing_index]

  # ==================================
  # Integrate
  trace <- trace %>%
    dplyr::mutate(
      weight = mass * gravity_const,
      force_net = force - weight,
      acceleration = force_net / mass
    ) %>%
    # Fix the acceleration
    dplyr::mutate(
      # Make acceleration in the flight phase equal to -g
      acceleration = dplyr::if_else(
        time >= take_off_time & time < landing_time,
        -gravity_const, acceleration
      ),
      # Make acceleration in the dropping phase equal to -g
      acceleration = dplyr::if_else(
        time < movement_start_time,
        -gravity_const, acceleration
      ),

      # Create "special" acceleration, which equals zero before the drop land
      acc_after_drop = dplyr::if_else(
        time < movement_start_time,
        0, acceleration
      ),
    ) %>%
    # Integrate velocity
    dplyr::mutate(
      velocity = integrate(time, acc_after_drop, cumulative = TRUE),
      height_from_start = integrate(time, velocity, cumulative = TRUE)
    )

  # Correct the velocity so that landing and dropping heights equal 0
  landing_height_from_start <- trace$height_from_start[trace$time == landing_time]

  velocity_diff <- landing_height_from_start / (landing_time - movement_start_time)

  trace <- trace %>%
    dplyr::mutate(velocity = velocity - velocity_diff)

  # Fix the velocity before the drop
  # Get the start velocity
  start_velocity <- trace$velocity[movement_start_index]

  if (start_velocity > 0) {
    warning("Estimated start velocity larger than zero. Please check the resulting trace for errors")
  }

  before_drop_trace <- trace %>%
    dplyr::filter(time <= movement_start_time) %>%
    dplyr::mutate(
      dropping_velocity = start_velocity + integrate(time, -acceleration, cumulative = TRUE, reverse = TRUE),
      dropping_velocity = dplyr::if_else(dropping_velocity > 0, 0, dropping_velocity)
    )

  # combine the two
  trace <- dplyr::left_join(
    trace, before_drop_trace[c("time", "dropping_velocity")],
    by = "time"
  )

  # Fix the velocity
  trace <- trace %>%
    dplyr::mutate(velocity = dplyr::if_else(is.na(dropping_velocity), velocity, dropping_velocity))

  # Now integrate to get height
  trace <- trace %>%
    dplyr::mutate(
      height_from_start = integrate(time, velocity, cumulative = TRUE)
    )

  # Find the height at the end time
  start_time_height <- trace$height_from_start[movement_start_index]

  # That height should be zero
  trace$height_from_start <- trace$height_from_start - start_time_height

  # Create height from take off origin variable
  take_off_height <- trace$height_from_start[trace$time >= take_off_time][1]
  trace <- trace %>%
    dplyr::mutate(
      height_from_take_off = height_from_start - take_off_height
    )

  # Return list
  list(
    trace = trace,
    movement_start_time = movement_start_time,
    take_off_time = take_off_time,
    landing_time = landing_time
  )
}
