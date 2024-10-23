library(hockeyR)
library(tidyverse)
library(sportyR)
library(ggplot2)
library(ggimage)
library(magick)
library(shiny)
library(glue)

pbp.test <- read.csv("MASTER CORSI.csv") #MODIFY
game_og <- pbp.test

ui <- fluidPage(
  titlePanel("MSU All Game Shot Plots"),
  absolutePanel(
    top = 10, right = 10,
    selectInput("periods", "Select Periods:", 
                choices = unique(game_og$Period), 
                selected = 3, 
                multiple = TRUE),
    selectInput("gameState", "",
                choices = c("Select Game State" = "all", 
                            "Leading" = "leading", 
                            "Trailing" = "trailing", 
                            "Tied" = "tied"))
  ),
  fluidRow(
    column(12, 
           plotOutput("shotPlot", height = "800px") 
    )
  )
)

server <- function(input, output) {
  output$shotPlot <- renderPlot({
    req(input$periods)
    
    game <- game_og %>%
      mutate(
        X = ifelse(Period == 2, X * -1, X),
        Y = ifelse(Period == 2, Y * -1, Y)
      )
    
    fenwick_events <- c("Miss", "Shot", "Goal", "Block")
    shots <- game %>%
      filter(Type %in% fenwick_events) %>%
      filter(Period %in% input$periods)
    
    if (input$gameState == "leading") {
      shots <- shots %>% filter(msu_score > opp_score)
    } else if (input$gameState == "trailing") {
      shots <- shots %>% filter(msu_score < opp_score)
    } else if (input$gameState == "tied") {
      shots <- shots %>% filter(msu_score == opp_score)
    }
    
    msu_corsi <- shots %>%
      filter(Strength == "EVEN") %>%
      filter(Team == "MSU") %>%
      summarise(count = n()) %>%
      pull(count)
    
    opp_corsi <- shots %>%
      filter(Strength == "EVEN") %>%
      filter(Team != "MSU") %>%
      summarise(count = n()) %>%
      pull(count)
    
    logo <- "msulogo.png"
    geom_hockey("nhl") +
      geom_image(
        aes(image = logo, x = 0, y = 0),
        size = 0.22, asp = 2.35
      ) +
      geom_point(
        data = shots,
        aes(X, Y, color = Team, shape = Type),
        size = 10
      ) +
      scale_shape_manual(values = c("Goal" = 19, "Block" = 7, "Shot" = 21, "Miss" = 3)) +
      scale_color_manual(values = c("MSU" = "#18453B", "BC" = "#98002E")) +
      labs(
        title = "MSU",
        subtitle = glue("Selected Periods: {paste(input$periods, collapse = ', ')}\n
                         5v5 Corsi {round((msu_corsi / (msu_corsi + opp_corsi)) * 100, 1)}%"),
        caption = "MSU Hockey Analytics"
      ) +
      theme(
        plot.title = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = .9)
      )
  })
}


shinyApp(ui = ui, server = server)