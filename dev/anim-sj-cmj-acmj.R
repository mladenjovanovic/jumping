library(tidyverse)
library(gganimate)
library(ggdist)
library(ggh4x)

# Squat Jump
SJ_df <- read_csv("dev/SJ-a.csv", skip = 9) %>%
  mutate(Force = Left + Right)

SJ_trace <-parse_SJ(
  time = SJ_df$Time,
  force = SJ_df$Force,
  mass = 102.52,
  start_threshold = 40)

# CMJ
CMJ_df <- read_csv("dev/CMJ.csv", skip = 9) %>%
  mutate(Force = Left + Right)

CMJ_trace <- parse_CMJ(
  time = CMJ_df$Time,
  force = CMJ_df$Force,
  mass = 102.54,
  start_threshold = 20)

# ACMJ (CMJ with arm swing)
ACMJ_df <- read_csv("dev/ACMJ.csv", skip = 9) %>%
  mutate(Force = Left + Right)

ACMJ_trace <- parse_CMJ(
  time = ACMJ_df$Time,
  force = ACMJ_df$Force,
  mass = 102.48,
  start_threshold = 20)

# Combine them together
jumps_trace <- rbind(
  SJ_trace$trace %>% mutate(jump = "SJ"),
  CMJ_trace$trace %>% mutate(jump = "CMJ"),
  ACMJ_trace$trace %>% mutate(jump = "ACMJ")
) %>%
  mutate(jump = factor(jump, levels = c("SJ", "CMJ", "ACMJ"))) %>%
  filter(time_from_take_off > -1.1 & time_from_take_off < 1.1)

jumps_phases <- rbind(
  SJ_trace$phases %>% mutate(jump = "SJ"),
  CMJ_trace$phases %>% mutate(jump = "CMJ"),
  ACMJ_trace$phases %>% mutate(jump = "ACMJ")
) %>%
  mutate(jump = factor(jump, levels = c("SJ", "CMJ", "ACMJ")))

jumps_moments <- jumps_trace %>%
  filter(!is.na(moment))

# Now fix the times
take_off_time <- jumps_moments %>%
  filter(moment == "Take off") %>%
  mutate(moment = "take_off_time") %>%
  pivot_wider(id_cols = "jump", values_from = "time", names_from = "moment")

jumps_phases <- jumps_phases %>%
  left_join(take_off_time, by = "jump") %>%
  mutate(
    start_time = start_time - take_off_time,
    stop_time = stop_time - take_off_time) %>%
  filter(phase %in% c(
    "Eccentric phase", "Concentric phase",
    "Flight phase", "Landing phase")) %>%
  mutate(phase = factor(phase, levels = c(
    "Eccentric phase", "Concentric phase",
    "Flight phase", "Landing phase")))

# Make long version
jumps_trace_long <- jumps_trace %>%
  rename(
    `Force (N)` = force,
    `Acceleration (m/s/s)` = acceleration,
    `Velocity (m/s)` = velocity,
    `Height (m)` = height_from_take_off) %>%
  pivot_longer(cols = c("Force (N)", "Acceleration (m/s/s)", "Velocity (m/s)", "Height (m)")) %>%
  mutate(name = factor(name, levels = c("Force (N)", "Acceleration (m/s/s)", "Velocity (m/s)", "Height (m)")))

jumps_moments_long <- jumps_moments %>%
  rename(
    `Force (N)` = force,
    `Acceleration (m/s/s)` = acceleration,
    `Velocity (m/s)` = velocity,
    `Height (m)` = height_from_take_off) %>%
  pivot_longer(cols = c("Force (N)", "Acceleration (m/s/s)", "Velocity (m/s)", "Height (m)")) %>%
  mutate(
    time = time_from_take_off,
    name = factor(name, levels = c("Force (N)", "Acceleration (m/s/s)", "Velocity (m/s)", "Height (m)")))

# Plot
phases_color <- c(
"Eccentric phase" = "#a6cee3",
"Concentric phase" = "#fdbf6f",
"Flight phase" = "#d9d9d9",
"Landing phase" = "#fb9a99"
)

gg <- jumps_trace_long %>%
  filter(name != "Acceleration (m/s/s)") %>%
  ggplot() +
  theme_ggdist() +
  geom_rect(
    data = jumps_phases,
    aes(xmin = start_time, xmax = stop_time, ymin = -Inf, ymax = Inf, fill = phase),
    alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(x = time_from_take_off, y = value, group = 1)) +
  #geom_point(
  #  data = jumps_moments_long %>%
  #  filter(name != "Acceleration (m/s/s)"),
  #  aes(x = time_from_take_off, y = value),
  #  shape = 21, size = 1,
  #  fill = "white"
  #) +
  facet_grid(name~jump, scales = "free_y") +
  #facet_grid2(jump~name, scales = "free_y", independent = "y") +
  ylab(NULL) +
  xlab("Time from take off (s)") +
  scale_fill_manual(values = phases_color) +
  theme(legend.position = "top", legend.title = element_blank())
gg

# Compare the lines
gg <- jumps_trace_long %>%
  filter(time_from_take_off <= 0) %>%
  ggplot() +
  theme_ggdist() +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(x = time_from_take_off, y = value, color = jump), alpha = 0.8) +
  facet_wrap(~name, scales = "free_y") +
  ylab(NULL) +
  xlab("Time from take off [s]") +
  theme(legend.position = "top", legend.title = element_blank())

gg

gg <- jumps_trace_long %>%
  filter(time_perc >= 0 & time_perc <= 100) %>%
  ggplot() +
  theme_ggdist() +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(x = time_perc, y = value, color = jump), alpha = 0.8) +
  facet_wrap(~name, scales = "free_y") +
  ylab(NULL) +
  xlab("Movement time [%]") +
  theme(legend.position = "top", legend.title = element_blank())

gg

# Force over distance/hight
gg <- jumps_trace %>%
  filter(phase %in% c("Concentric phase")) %>%
  ggplot() +
  theme_ggdist() +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = 100, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = mean(jumps_trace$weight), linetype = "solid", alpha = 0.3) +
  geom_line(aes(x = height_from_take_off, y = force, color = jump), alpha = 0.8) +
  ylab("Force [N]") +
  xlab("Height from take-off [m]") +
  theme(legend.position = "top", legend.title = element_blank())

gg

# Force-Velocity
gg <- jumps_trace %>%
  filter(phase %in% c("Concentric phase")) %>%
  ggplot() +
  theme_ggdist() +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = 100, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = mean(jumps_trace$weight), linetype = "solid", alpha = 0.3) +
  geom_line(aes(x = height_from_take_off, y = force, color = jump), alpha = 0.8) +
  ylab("Force [N]") +
  xlab("Height from take-off [m]") +
  theme(legend.position = "top", legend.title = element_blank())

gg

# Animate
stop()
aa <- gg +
  transition_reveal(time_from_take_off)

vv <- animate(aa, duration = 8*2.2, nframes = 8*2.2 * 30, height = 8, width = 8, units = "in", res = 300, renderer = av_renderer())

anim_save("dev/anim.mp4", vv)
