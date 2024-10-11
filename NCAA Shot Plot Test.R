library(hockeyR)
library(tidyverse)
library(sportyR)
library(ggplot2)
library(ggimage)
library(magick)

pbp.test <- read.csv("hockey_plot_test.csv")
game <- pbp.test
img <- 
fenwick_events <- c("MISSED_SHOT","Shot","Goal")
shots <- game %>% filter(Type %in% fenwick_events)

logo <- "msulogo.png"
geom_hockey("nhl") +
  ggimage::geom_image(
    aes(image = logo, x=0,y=0),
    size = 0.22, asp = 2.35
  )+
  geom_point(
    data = shots,
    aes(X, Y, color=Team),
    size = 6,
    shape = ifelse(shots$Type == "Goal", 19, 1)
  )+
  scale_color_manual(values = c("Home" = "darkgreen", "Away" = "navy"))+
  labs(
    title = glue::glue("Test @ Test"),
    subtitle = glue::glue(
      "Test Game Date\n
      Test1 3 - 2 Test2"
    ),
    caption = "data from shot.plotter | plot made with sportR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )
ggsave("Test Shot Plot.pdf")