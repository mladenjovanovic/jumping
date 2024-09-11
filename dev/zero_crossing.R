df <- dplyr::tibble(
  t = seq(0, pi, length = 100),
  f = sin(t) + 0.5 * sin(3 * t) + 0.45 * sin(5 * t) +  0.25 * sin(7 * t) - 0.65,
)

gg <- df %>%
  ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_point(ggplot2::aes(x = t, y = f))

gg

# Where does the y crosses the 0? Return interpolated location using x
cross0 <- zero_crossings(x = df$t, y = df$f)

gg <- gg +
  ggplot2::geom_vline(xintercept = cross0, color = "red")

# When using only x, function returns interpolated location in x
# This is because the acutal x observation might not be located on 0
zero_crossings(x = df$f)

# If you want to pull the closes points, make sure to round the indices
# to closes integer (using either round, floor, or ceiling functions)

cross0 <- zero_crossings(x = df$f)

floor_i <- floor(cross0)
ceiling_i <- ceiling(cross0)

closest_obs <- c(floor_i, ceiling_i)

gg +
  theme_classic() +
  ggplot2::geom_point(
    data = df[closest_obs, ],
    ggplot2::aes(x = t, y = f),
    shape = 21,
    fill = "white",
    color = "red"
  )

