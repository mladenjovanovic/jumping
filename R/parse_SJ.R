parse_SJ <- function(time,
                      force,
                      mass,
                      g = 9.80665,
                      start_threshold = 20,
                      contact_threshold = 20,
                      na.rm = FALSE) {

  trace <- data.frame(
    time = time,
    force = force
  )

  if (na.rm == TRUE) trace <- trace %>% stats::na.omit()

  # =============================================
  # Find flight time and location
  flight_phase_index <- longest_TRUE_streak(trace$force < contact_threshold)
  flight_phase_time <- trace$time[flight_phase_index]

  take_off_index <- flight_phase_index[1]
  take_off_time <- trace$time[take_off_index]

  # =============================================
  # Find start of CMJ
  before_jump_trace <- trace %>%
    dplyr::filter(
      time < take_off_time
    )

  # Find peak force
  max_force_index <- which.max(before_jump_trace$force)
  max_force_time <- before_jump_trace$time[max_force_index]
  max_force_value <- before_jump_trace$force[max_force_index]

  # Filer trace before peak force
  before_peak_trace <- trace %>%
    dplyr::filter(
      time < max_force_time
    )

  # Find start of the motion
  upper_start_threshold <- mass * g + start_threshold
  lower_start_threshold <- mass * g - start_threshold

  propulsive_phase_index <- longest_TRUE_streak(
    before_peak_trace$force <= upper_start_threshold &
      before_peak_trace$force >= lower_start_threshold)

  propulsive_phase_time <- before_peak_trace$time[propulsive_phase_index]
  movement_start_time <-  propulsive_phase_time[2]

  # ----- Orig
  #propulsive_phase_index <- longest_TRUE_streak(before_peak_trace$force > upper_start_threshold)
  #propulsive_phase_time <- before_peak_trace$time[propulsive_phase_index]
  #movement_start_time <-  propulsive_phase_time[1]

  # =============================================
  # Create kinematics traces
  trace <- trace %>%
    dplyr::mutate(
      weight = mass * g,
      force_net = force - weight,
      acceleration = force_net / mass
    ) %>%
    # Make acceleration before movement start time equal to 0
    dplyr::mutate(
      acceleration = dplyr::if_else(time < movement_start_time, 0, acceleration)
    ) %>%
    # Make acceleration during flight time equal to -gravity
    dplyr::mutate(
      acceleration = dplyr::if_else(
        time >= flight_phase_time[1] & time <= flight_phase_time[2],
        -g, acceleration)
    ) %>%
    # Create velocity and height assuming v = 0 and h = 0 at the instant of
    # movement start
    dplyr::mutate(
      velocity = integrate(time, acceleration, cumulative = TRUE),
      height_from_start = integrate(time, velocity, cumulative = TRUE)
    ) %>%
    # Create different time and distance origin variables
    dplyr::mutate(
      time_from_start = time - movement_start_time,
      time_from_take_off = time - take_off_time,
      time_perc = 100 * (time - movement_start_time) / (take_off_time - movement_start_time)
    ) %>%
    # Create net impulse and power
    dplyr::mutate(
      impulse = integrate(time, force, cumulative = TRUE),
      impulse_net = integrate(time, force_net, cumulative = TRUE),
      power = force * velocity,
      work = integrate(time, power, cumulative = TRUE)
    )

  take_off_height <- trace$height_from_start[take_off_index]

  # Create height from take off origin variable
  trace <- trace %>%
    dplyr::mutate(
      height_from_take_off = height_from_start - take_off_height
    )

  # =============================================
  # Mark key moments and phases
  max_upward_velocity_time <- trace$time[[which.max(trace$velocity)]]

  concentric_trace <- trace %>%
    dplyr::filter(time < max_upward_velocity_time)

  jump_peak_time <- trace$time[[which.max(trace$height_from_start)]]
  landing_time <- trace$time[[flight_phase_index[2] + 1]]
  impact_peak_time <- trace$time[[which.max(trace$force)]]

  landing_trace <- trace %>%
    dplyr::filter(time > impact_peak_time)

  catch_time <- landing_trace$time[[which.min(landing_trace$height_from_start)]]

  start_time <- trace$time[1]
  stop_time <- trace$time[length(trace$time)]

  # Moments
  moments_df <- dplyr::tribble(
    ~moment, ~time,
    "Movement Start", movement_start_time,
    "Max Upward Velocity", max_upward_velocity_time,
    "Take off", take_off_time,
    "Peak of the jump", jump_peak_time,
    "Landing", landing_time,
    "Impact peak", impact_peak_time,
    "Catch", catch_time
  )

  trace <- dplyr::left_join(
    trace,
    moments_df, by = "time"
  )

  # Phases
  phases_df <- dplyr::tribble(
    ~phase, ~start_time, ~stop_time,
    "Before SJ", start_time, movement_start_time,
    "Concentric phase", movement_start_time, take_off_time,
    "Flight phase", take_off_time, landing_time,
    "Landing phase", landing_time, catch_time,
    "After SJ", catch_time, stop_time,
    "", stop_time, Inf
  )

  trace$phase <- cut(
    trace$time,
    breaks = phases_df$start_time,
    labels = head(phases_df$phase, -1),
    include.lowest = TRUE)

  phases_df <- head(phases_df, -1)
  trace$phase <- factor(trace$phase, levels = phases_df$phase)
  phases_df$phase <- factor(phases_df$phase, levels = phases_df$phase)

  # Sub Phases
  sub_phases_df <- dplyr::tribble(
    ~sub_phase, ~start_time, ~stop_time,
    "Before SJ", start_time, movement_start_time,
    "Propulsive phase [speeding]", movement_start_time, max_upward_velocity_time,
    "Propulsive phase [slowing]", max_upward_velocity_time, take_off_time,
    "Flight phase [ascent]", take_off_time, jump_peak_time,
    "Flight phase [descent]", jump_peak_time, landing_time,
    "Landing phase [impact]", landing_time, impact_peak_time,
    "Landing phase (stabilization)", impact_peak_time, catch_time,
    "After SJ", catch_time, stop_time,
    "", stop_time, Inf
  )

  trace$sub_phase <- cut(
    trace$time,
    breaks = sub_phases_df$start_time,
    labels = head(sub_phases_df$sub_phase, -1),
    include.lowest = TRUE)

  sub_phases_df <- head(sub_phases_df, -1)
  trace$sub_phase <- factor(trace$sub_phase, levels = sub_phases_df$sub_phase)
  sub_phases_df$sub_phase <- factor(sub_phases_df$sub_phase, levels = sub_phases_df$sub_phase)

  # Return
  list(trace = trace, moments = moments_df, phases = phases_df, sub_phases = sub_phases_df)
}
