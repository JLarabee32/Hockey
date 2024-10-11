library(hockeyR)
library(tidyverse)
library(sportyR)
library(ggplot2)
library(ggimage)
library(magick)
library(dplyr)
library(paletteer)
library(RColorBrewer)

colourCount = 25

palette_opp <- colorRampPalette(colors = c("white", "red"))(10)
palette_msu <- colorRampPalette(colors = c("white", "#18453B"))(10)
scales::show_col(palette_msu)
getPalette_msu = colorRampPalette(palette_msu)
getPalette_opp = colorRampPalette(palette_opp)
team_colors <- c("Home" = palette_msu, "Away" = palette_opp)

scales::show_col(palette_opp)
pbp.test <- read.csv("hockey_plot_test.csv")
game <- pbp.test
fenwick_events <- c("MISSED_SHOT","Shot","Goal")
shots <- game %>% filter(Type %in% fenwick_events)
shots_msu <- filter(shots, Team == "Home")
shots_opp <- filter(shots, Team == "Away")

logo <- "msulogo.png"
gruff_logo <- "gruff_sparty.jpg"

geom_hockey("nhl", display_range = "defensive_zone")+
  ggimage::geom_image(
    aes(image = gruff_logo, x=-45,y=0),
    size = 0.08, asp = 2.35
  )+
  geom_density2d_filled(
    data = shots_msu,
    aes(x = X, y = Y, fill = ..level.., color=..level..),
    contour_var = "ndensity",
    alpha = 0.9,
    linetype = 0,
    breaks = seq(0.1,1.0, length.out=10))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(getPalette_msu(colourCount)), aesthetics = c("fill", "color"))

ggsave("MSU Test Heat Map.pdf")

geom_hockey("nhl", display_range = "offensive_zone")+
  ggimage::geom_image(
    aes(image = logo, x=0,y=0),
    size = 0.22, asp = 2.35
  )+ 
  geom_density2d_filled(
    data = shots_opp,
    aes(x = X, y = Y, fill = ..level.., color=..level..),
    contour_var = "ndensity",
    alpha = 0.9,
    linetype = 0,
    breaks = seq(0.1,1.0, length.out=10))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(getPalette_opp(colourCount)), aesthetics = c("fill", "color"))

ggsave("Opponent Test Heat Map.pdf")

  