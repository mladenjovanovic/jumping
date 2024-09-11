library(tidyverse)
library(segmented)
library(plotly)

cmj_df <- read_csv("dev/abalakov.csv", skip = 9)

cmj_df <- cmj_df %>%
  mutate(Force = Left + Right)

g <- 9.81
BM <- 102.48
BW <- BM * g

gg <- cmj_df %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = Force)) +
  geom_hline(yintercept = c(BW), linetype = "dotted", alpha = 0.7)


# Find initiation of the jump
flight_phase_index <- longest_TRUE_streak(cmj_df$Force < 20)
flight_phase_time <- cmj_df$Time[flight_phase_index]


gg <- gg +
  annotate(
    "rect",
    xmin = flight_phase_time[1], xmax = flight_phase_time[2],
    ymin = -20, ymax = 20,
    alpha = 0.5)

ggplotly(gg)
