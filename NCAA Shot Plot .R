library(hockeyR)
library(tidyverse)
library(sportyR)
library(ggplot2)
library(ggimage)
library(magick)

pbp.test <- read.csv("10_11_24 MSUvBC Full Game.csv") #MODIFY
game_og <- pbp.test
game <- game_og %>%
  mutate(X = ifelse(Period == 2, X*-1, X))
fenwick_events <- c("Miss","Shot","Goal", "Block")
shots <- game %>% filter(Type %in% fenwick_events)

logo <- "msulogo.png"
geom_hockey("nhl") +
  ggimage::geom_image(
    aes(image = logo, x=0,y=0),
    size = 0.22, asp = 2.35
  )+
  geom_point(
    data = shots,
    aes(X, Y, color=Team, shape=Type),
    size = 8,
  )+
  scale_shape_manual(values = c("Goal" = 19, "Block" = 7, "Shot" = 21, "Miss" = 3))+
  scale_color_manual(values = c("MSU" = "#18453B", "BC" = "#98002E"))+
  labs(
    title = glue::glue("MSU vs BC"), #MODIFY
    subtitle = glue::glue(
      "10/2/24\n 
      MSU 0 - 3 BC" #MODIFY
    ),
    caption = "10/11/24"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )
ggsave("10.12 BC Fri Shot Plot.pdf")
