library(tidyverse)
library(plotly)
library(signal)
library(zoo)

cmj_df <- read_csv("dev/abalakov.csv", skip = 9)

cmj_df <- cmj_df %>%
  mutate(Force = Left + Right)

sampleFreq <- 1000
butterFilter <- signal::butter(n = 2, W = (10)/(sampleFreq/2), type = "low", plane = "z")
recoveredSig <- signal::filter(butterFilter, cmj_df$Force)

cmj_df$butter_Force <- recoveredSig

cmj_df$smoothed_Force <- zoo::rollmean(cmj_df$Force, k = 20, fill = NA)

gg <- cmj_df %>%
  ggplot(aes(x = Time)) +
  geom_line(aes(y = Force), alpha = 0.8) +
  geom_line(aes(y = butter_Force), alpha = 0.8, color = "red") +
  geom_line(aes(y = smoothed_Force), alpha = 0.8, color = "blue")

gg

ggplotly(gg)
