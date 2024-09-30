summarise_trace <- function(trace, left_force = NULL, right_force = NULL) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  moment <- NULL
  phase <- NULL
  variable <- NULL
  summarise <- NULL
  value <- NULL
  start <- NULL
  median <- NULL
  sub_phase <- NULL
  difference <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  trace_long <- trace %>%
    tidyr::pivot_longer(cols = -c("moment", "phase", "sub_phase"), names_to = "variable", values_to = "value")

  by_moment <- trace_long %>%
    dplyr::filter(!is.na(moment))

  by_phase <- trace_long %>%
    dplyr::group_by(phase, variable) %>%
    summarise(
      start = utils::head(value, 1),
      stop = utils::tail(value, 1),
      difference = stop - start,
      min = min(value),
      max = max(value),
      range = max - min,
      mean = mean(value),
      median = median(value),
      SD = stats::sd(value),
      IQR = stats::IQR(value)
    ) %>%
    dplyr::ungroup()

  by_sub_phase <- trace_long %>%
    dplyr::group_by(sub_phase, variable) %>%
    summarise(
      start = utils::head(value, 1),
      stop = utils::tail(value, 1),
      difference = stop - start,
      min = min(value),
      max = max(value),
      range = max - min,
      mean = mean(value),
      median = median(value),
      SD = stats::sd(value),
      IQR = stats::IQR(value)
    ) %>%
    dplyr::ungroup()

  # Provide key metrics
  # =======================

  # General info
  # -----------------
  mass <- trace$mass[1]
  weight <- trace$weight[1]
  gravity_const <- trace$gravity_const[1]

  # Phases duration
  # -----------------
  eccentric_phase_duration <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "time") %>%
    dplyr::pull(difference)

  unweighting_phase_duration <- by_sub_phase %>%
    dplyr::filter(sub_phase %in% c("Unweighting phase [speeding]", "Unweighting phase [slowing]"), variable == "time") %>%
    dplyr::pull(difference) %>%
    sum()

  breaking_phase_duration <- by_sub_phase %>%
    dplyr::filter(sub_phase == "Breaking phase", variable == "time") %>%
    dplyr::pull(difference)

  concentric_phase_duration <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "time") %>%
    dplyr::pull(difference)

  if (length(eccentric_phase_duration) == 0) {
    movement_duration <- concentric_phase_duration
  } else {
    movement_duration <- eccentric_phase_duration + concentric_phase_duration
  }

  flight_duration <- by_phase %>%
    dplyr::filter(phase == "Flight phase", variable == "time") %>%
    dplyr::pull(difference)

  landing_duration <- by_phase %>%
    dplyr::filter(phase == "Landing phase", variable == "time") %>%
    dplyr::pull(difference)

  # Forces
  # -----------------
  concentric_peak_force <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "force") %>%
    dplyr::pull(max)

  eccentric_minimum_force <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "force") %>%
    dplyr::pull(min)

  eccentric_peak_force <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "force") %>%
    dplyr::pull(max)

  eccentric_end_force <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "force") %>%
    dplyr::pull(stop)

  landing_peak_force <- by_phase %>%
    dplyr::filter(phase == "Landing phase", variable == "force") %>%
    dplyr::pull(max)

  # Impulse used to calculate mean forces
  concentric_impulse <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "impulse") %>%
    dplyr::pull(difference)

  concentric_impulse_net <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "impulse_net") %>%
    dplyr::pull(difference)

  eccentric_impulse <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "impulse") %>%
    dplyr::pull(difference)

  eccentric_impulse_net <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "impulse_net") %>%
    dplyr::pull(difference)

  landing_impulse <- by_phase %>%
    dplyr::filter(phase == "Landing phase", variable == "impulse") %>%
    dplyr::pull(difference)

  landing_impulse_net <- by_phase %>%
    dplyr::filter(phase == "Landing phase", variable == "impulse_net") %>%
    dplyr::pull(difference)

  concentric_mean_force <- concentric_impulse / concentric_phase_duration
  eccentric_mean_force <- eccentric_impulse / eccentric_phase_duration
  landing_mean_force <- landing_impulse / landing_duration

  # Velocities
  # -----------------
  eccentric_minimum_velocity <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "velocity") %>%
    dplyr::pull(min)

  concentric_peak_velocity <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "velocity") %>%
    dplyr::pull(max)

  take_off_velocity <- by_phase %>%
    dplyr::filter(phase == "Flight phase", variable == "velocity") %>%
    dplyr::pull(start)


  # Displacements
  # -----------------

  eccentric_phase_displacement <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "height_from_start") %>%
    dplyr::pull(difference)

  concentric_phase_displacement <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "height_from_start") %>%
    dplyr::pull(difference)

  jump_height_imp_mom <- by_phase %>%
    dplyr::filter(phase == "Flight phase", variable == "height_from_take_off") %>%
    dplyr::pull(max)

  jump_height_flight_time <- height_from_FT(
    flight_time = flight_duration,
    gravity_const = gravity_const
  )

  # Displacement from the start height
  take_off_displacement <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "height_from_start") %>%
    dplyr::pull(stop)

  landing_displacement <- by_phase %>%
    dplyr::filter(phase == "Landing phase", variable == "height_from_start") %>%
    dplyr::pull(start)

  # Calculate velocity
  concentric_mean_velocity <- concentric_phase_displacement / concentric_phase_duration
  eccentric_mean_velocity <- eccentric_phase_displacement / eccentric_phase_duration

  # Power
  # -----------------
  concentric_peak_power <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "power") %>%
    dplyr::pull(max)

  eccentric_minimum_power <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "power") %>%
    dplyr::pull(min)

  concentric_work <- by_phase %>%
    dplyr::filter(phase == "Concentric phase", variable == "work") %>%
    dplyr::pull(difference)

  eccentric_work <- by_phase %>%
    dplyr::filter(phase == "Eccentric phase", variable == "work") %>%
    dplyr::pull(difference)

  concentric_mean_power <- concentric_work / concentric_phase_duration
  eccentric_mean_power <- eccentric_work / eccentric_phase_duration

  # Additional variables for mean force over distance
  concentric_mean_force_distance <- concentric_work / concentric_phase_displacement
  eccentric_mean_force_distance <- eccentric_work / eccentric_phase_displacement

  # Composite variables
  # -----------------

  landing_take_off_displacement_difference <- landing_displacement - take_off_displacement
  explosive_index <- take_off_velocity / movement_duration
  RSI <- jump_height_imp_mom / movement_duration
  RSImod <- flight_duration / movement_duration
  ecc_conc_ratio <- eccentric_phase_duration / concentric_phase_duration

  eccentric_stiffness <- -(eccentric_end_force - eccentric_minimum_force) / eccentric_phase_displacement

  # Return object
  # ==============================
  list(
    summary_tbl = list(
      moments = by_moment,
      phases = by_phase,
      sub_phases = by_sub_phase
    ),

    # Key metrics
    # ====================
    metrics = list(
      # General info
      mass = mass,
      gravity_const = gravity_const,
      weight = weight,

      # Phases
      movement_duration = movement_duration,
      eccentric_phase_duration = eccentric_phase_duration,
      concentric_phase_duration = concentric_phase_duration,
      unweighting_phase_duration = unweighting_phase_duration,
      breaking_phase_duration = breaking_phase_duration,
      flight_duration = flight_duration,
      landing_duration = landing_duration,

      # Forces
      concentric_peak_force = concentric_peak_force,
      eccentric_minimum_force = eccentric_minimum_force,
      eccentric_peak_force = eccentric_peak_force,
      eccentric_end_force = eccentric_end_force,
      landing_peak_force = landing_peak_force,
      concentric_mean_force = concentric_mean_force,
      concentric_mean_force_distance = concentric_mean_force_distance,
      eccentric_mean_force = eccentric_mean_force,
      eccentric_mean_force_distance = eccentric_mean_force_distance,
      landing_mean_force = landing_mean_force,

      # Impulses
      concentric_impulse = concentric_impulse,
      concentric_impulse_net = concentric_impulse_net,
      eccentric_impulse = eccentric_impulse,
      eccentric_impulse_net = eccentric_impulse_net,


      # Velocities
      eccentric_minimum_velocity = eccentric_minimum_velocity,
      concentric_peak_velocity = concentric_peak_velocity,
      take_off_velocity = take_off_velocity,
      concentric_mean_velocity = concentric_mean_velocity,
      eccentric_mean_velocity = eccentric_mean_velocity,

      # Work
      concentric_work = concentric_work,
      eccentric_work = eccentric_work,

      # Power
      concentric_peak_power = concentric_peak_power,
      eccentric_minimum_power = eccentric_minimum_power,
      concentric_mean_power = concentric_mean_power,
      eccentric_mean_power = eccentric_mean_power,

      # Displacements
      eccentric_phase_displacement = eccentric_phase_displacement,
      concentric_phase_displacement = concentric_phase_displacement,
      jump_height_imp_mom = jump_height_imp_mom,
      jump_height_flight_time = jump_height_flight_time,
      take_off_displacement = take_off_displacement,
      landing_displacement = landing_displacement,

      # Composite variables
      landing_take_off_displacement_difference = landing_take_off_displacement_difference,
      explosive_index =  explosive_index,
      RSI = RSI,
      RSImod = RSImod,
      ecc_conc_ratio = ecc_conc_ratio,
      eccentric_stiffness = eccentric_stiffness
    ),

    # Assymetry metrics
    assymetry = list()
  )
}
