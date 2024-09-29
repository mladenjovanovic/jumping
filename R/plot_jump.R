plot_jump <- function(jump,
                      x_var = "time_from_take_off",
                      variables = c("force", "acceleration", "velocity", "height_from_take_off"),
                      plot_phases = TRUE,
                      plot_moments = TRUE) {

  # Solution for "no visible binding for global variable" note
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  variable <- NULL
  phase <- NULL
  .data <- NULL
  value <- NULL
  start <- NULL
  min_value <- NULL
  max_value <- NULL
  moment <- NULL
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  trace <- jump$trace

  trace_long <- trace %>%
    tidyr::pivot_longer(cols = variables, names_to = "variable", values_to = "value") %>%
    dplyr::mutate(
      variable = factor(variable, levels = variables)
    )

  # Phases
  phases_long <- trace_long %>%
    dplyr::group_by(phase, variable) %>%
    dplyr::summarise(
      start = min(.data[[x_var]]),
      stop = max(.data[[x_var]])
    ) %>%
    dplyr::ungroup()

  values_range <- trace_long %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      max_value = max(value),
      min_value = min(value)
    ) %>%
    dplyr::ungroup()

  phases_long <- phases_long %>%
    dplyr::left_join(values_range, by = "variable")

  gg <- trace_long %>%
    ggplot2::ggplot()

  if (plot_phases) {
    gg <- gg +
      ggplot2::geom_rect(
        data = phases_long,
        ggplot2::aes(xmin = start, xmax = stop, ymin = min_value, ymax = max_value, fill = phase),
        alpha = 0.5
      )
  }

  gg <- gg +
    ggplot2::geom_line(ggplot2::aes_string(x = x_var, y = "value"), linewidth = 0.75)

  if (plot_moments) {
    # key moments
    moments_long <- trace_long %>%
      dplyr::filter(!is.na(moment))

    gg <- gg +
      ggplot2::geom_point(
        data = moments_long,
        ggplot2::aes_string(x = x_var, y = "value"),
        shape = 21,
        size = 1,
        fill = "white"
      ) #+
      #ggplot2::scale_shape_manual(na.value = NA, values = rep(21, 100))
  }

  gg <- gg +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(x_var)


  gg
}
