library(tidyverse)
library(plotly)
library(zoo)

cmj_df <- read_csv("dev/SJ-a.csv", skip = 9)

cmj_df <- cmj_df %>%
  mutate(Force = Left + Right)

g <- 9.81
BM <- 102.52
BW <- BM * g

gg <- cmj_df %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = Force)) +
  geom_hline(yintercept = c(BW), linetype = "dotted", alpha = 0.7)
ggplotly(gg)

# Parse CMJ
df <- parse_SJ(time = cmj_df$Time, force = cmj_df$Force, mass = BM, na.rm = F, start_threshold = 20)
max(df$trace$height_from_take_off, na.rm = TRUE)

df_long <- df$trace %>%
  pivot_longer(cols = c("force", "acceleration", "velocity", "height_from_take_off")) %>%
  mutate(name = factor(name, levels = c("force", "acceleration", "velocity", "height_from_take_off")))

min_max_df <- df_long %>%
  group_by(name) %>%
  summarise(
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE)
  ) %>%
  ungroup()

major_phases <- expand_grid(
  df$sub_phases,
  min_max_df
)

gg <- df_long %>%
  ggplot() +
  geom_rect(data = major_phases, aes(xmin = start_time, xmax = stop_time, ymin = ymin, ymax = ymax, fill = sub_phase), color = "black", alpha = 0.2) +
  geom_line(aes(x = time, y = value)) +
  facet_wrap(~name, scales = "free_y", ncol = 1)

gg

ggplotly(gg + theme(legend.position = "none"))

