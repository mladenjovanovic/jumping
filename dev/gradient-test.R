df <- dplyr::tibble(
  x = seq(0, 2 * pi, length = 100),
  y = sin(x) + 0.75 * sin(3 * x) + 0.45 * sin(5 * x) +  0.25 * sin(7 * x),
  grad = gradient(x, y)
)

df %>%
  ggplot2::ggplot(ggplot2::aes(x = x)) +
  ggplot2::geom_point(ggplot2::aes(y = y)) +
  ggplot2::geom_line(ggplot2::aes(y = grad), color = "red")

# Find a gradient for a specific value of x
gradient(df$x, df$y, x_value = 2)
