parse_DJ <- function(time,
                     force,
                     mass,
                     gravity_const = 9.80665,
                     contact_threshold = 20,
                     method = c("landing-heights", "flight-time", "ft-landing-combined", "known-end"),
                     end_time = NULL,
                     na.rm = FALSE) {
  if (method[1] == "known-end") {
    # Known end method
    if (is.null(end_time)) {
      stop("Missing `end_time` argument for the `known-end` method, please provide the time of the known stand still")
    }

    res <- parse_DJ_known_end(
      time = time,
      force = force,
      mass = mass,
      gravity_const = gravity_const,
      contact_threshold = contact_threshold,
      end_time = end_time,
      na.rm = na.rm
    )
  } else if (method[1] == "landing-heights") {
    # Landing heights method
    if (!is.null(end_time)) {
      warning("Argument `end_time` is provided, but not used in `landing-heights` method. Please use `known-end` method")
    }

    res <- parse_DJ_same_landing_height(
      time = time,
      force = force,
      mass = mass,
      gravity_const = gravity_const,
      contact_threshold = contact_threshold,
      na.rm = na.rm
    )
  } else if (method[1] == "flight-time") {
    # Flight time method
    if (!is.null(end_time)) {
      warning("Argument `end_time` is provided, but not used in `flight-time` method. Please use `known-end` method")
    }
    res <- parse_DJ_flight_time(
      time = time,
      force = force,
      mass = mass,
      gravity_const = gravity_const,
      contact_threshold = contact_threshold,
      na.rm = na.rm
    )
  } else if (method[1] == "ft-landing-combined") {
    # Combined method
    if (!is.null(end_time)) {
      warning("Argument `end_time` is provided, but not used in `ft-landing-combined` method. Please use `known-end` method")
    }

    res <- parse_DJ_combined(
      time = time,
      force = force,
      mass = mass,
      gravity_const = gravity_const,
      contact_threshold = contact_threshold,
      na.rm = na.rm
    )
  } else {
    stop("Unknown method to parse depth-jump. Please use `landing-heights`, `flight-time`, `ft-landing-combined`, or `known-end`")
  }

  # =============================================
  # Mark key moments and phases

  trace <- res$trace

  drop_start_index <- which(trace$velocity != 0)[1]
  drop_start_time <- trace$time[drop_start_index]

  drop_landing_time <- res$movement_start_time

  movement_trace <- trace[trace$time <= res$take_off_time & trace$time >= res$movement_start_time, ]

  max_downward_velocity_index <- which.min(movement_trace$velocity)
  max_downward_velocity_time <- movement_trace$time[max_downward_velocity_index]

  lowest_point_index <- which.min(movement_trace$height_from_take_off)
  lowest_point_time <- movement_trace$time[lowest_point_index]

  max_upward_velocity_index <- which.max(movement_trace$velocity)
  max_upward_velocity_time <- movement_trace$time[max_upward_velocity_index]

  flight_trace <- trace[trace$time >= res$take_off_time & trace$time <= res$landing_time, ]
  jump_peak_index <- which.max(flight_trace$height_from_take_off)
  jump_peak_time <- flight_trace$time[jump_peak_index]

  landing_trace <- trace[trace$time >= res$landing_time, ]
  impact_peak_index <- which.max(landing_trace$force)
  impact_peak_time <- landing_trace$time[impact_peak_index]

  catch_index <-  which.min(landing_trace$height_from_take_off)
  catch_time <- landing_trace$time[catch_index]

  start_time <- trace$time[1]
  stop_time <- trace$time[length(trace$time)]

  take_off_time <- res$take_off_time
  landing_time <- res$landing_time

  # Add time from take off
  trace$time_from_take_off <- trace$time - take_off_time

  # Moments
  moments_df <- dplyr::tribble(
    ~moment, ~time,
    "Drop Start", drop_start_time,
    "Drop Landing", drop_landing_time,
    "Max Downward Velocity", max_downward_velocity_time,
    "Lowest Point", lowest_point_time,
    "Max Upward Velocity", max_upward_velocity_time,
    "Take off", take_off_time,
    "Peak of the Jump", jump_peak_time,
    "Landing", landing_time,
    "Impact peak", impact_peak_time,
    "Catch", catch_time
  )

  trace <- dplyr::left_join(
    trace,
    moments_df,
    by = "time"
  )

  # Phases
  phases_df <- dplyr::tribble(
    ~phase, ~start_time, ~stop_time,
    "Before DJ", start_time, drop_start_time,
    "Dropping phase", drop_start_time, drop_landing_time,
    "Eccentric phase", drop_landing_time, lowest_point_time,
    "Concentric phase", lowest_point_time, take_off_time,
    "Flight phase", take_off_time, landing_time,
    "Landing phase", landing_time, catch_time,
    "After DJ", catch_time, stop_time,
    "", stop_time, Inf
  )

  trace$phase <- cut(
    trace$time,
    breaks = phases_df$start_time,
    labels = utils::head(phases_df$phase, -1),
    include.lowest = TRUE
  )

  phases_df <- utils::head(phases_df, -1)
  trace$phase <- factor(trace$phase, levels = phases_df$phase)
  phases_df$phase <- factor(phases_df$phase, levels = phases_df$phase)

  # Sub Phases
  sub_phases_df <- dplyr::tribble(
    ~sub_phase, ~start_time, ~stop_time,
    "Before DJ", start_time, drop_start_time,
    "Dropping phase", drop_start_time, drop_landing_time,
    "Unweighting phase", drop_landing_time, max_downward_velocity_time,
    "Breaking phase", max_downward_velocity_time, lowest_point_time,
    "Propulsive phase [speeding]", lowest_point_time, max_upward_velocity_time,
    "Propulsive phase [slowing]", max_upward_velocity_time, take_off_time,
    "Flight phase [ascent]", take_off_time, jump_peak_time,
    "Flight phase [descent]", jump_peak_time, landing_time,
    "Landing phase [impact]", landing_time, impact_peak_time,
    "Landing phase [stabilization]", impact_peak_time, catch_time,
    "After DJ", catch_time, stop_time,
    "", stop_time, Inf
  )

  trace$sub_phase <- cut(
    trace$time,
    breaks = sub_phases_df$start_time,
    labels = utils::head(sub_phases_df$sub_phase, -1),
    include.lowest = TRUE
  )

  sub_phases_df <- utils::head(sub_phases_df, -1)
  trace$sub_phase <- factor(trace$sub_phase, levels = sub_phases_df$sub_phase)
  sub_phases_df$sub_phase <- factor(sub_phases_df$sub_phase, levels = sub_phases_df$sub_phase)

  # Return
  list(trace = trace, moments = moments_df, phases = phases_df, sub_phases = sub_phases_df)
}
