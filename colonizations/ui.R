##### FINAL APP UI #####

library(countrycode)
library(plotly)
library(colorspace)
library(markdown)
library(sf)
library(rworldmap)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(tidyverse)

# Read in Data 
wvs_colonies <- readRDS("wvs_colonies.rds")
plot_map <- readRDS("plot_map.rds")
colonies <- readRDS("colonies.rds")
gini <- readRDS("gini_long.rds")
gdp <- readRDS("gdp_colonies.rds")
density_count <- readRDS("density_count.rds")
rights <- readRDS("rights.rds")


# Setting UI
ui <- fluidPage(
  # Theme
  theme = shinytheme("readable"),
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  
  # App options at top of page
  navbarPage("The Afterlife of Empire",
             # World map
             tabPanel("World Map", 
                      fluidRow(column(12, 
                               selectInput(inputId = "chosen_colonizer", 
                                           label = "Choose Your Empire",
                                           choices = c("Belgium", "Britain", "France", "Germany", "Italy", "Netherlands", "Portugal", "Spain", "None"),
                                           multiple = TRUE,
                                           selected = NULL))),
                      fluidRow(plotOutput("map")),
                      fluidRow(column(8, offset = 2, 
                                      htmlOutput("map_text")))
                      ),
             # Box plot
             tabPanel("Empires Over Time",
                        tabPanel("Empire Size Over Time",
                                 fluidRow(column(12, htmlOutput("page_break1"))),
                                 fluidRow(
                                   column(8, plotlyOutput("timeline", height = "550px")),
                                   column(4, htmlOutput("timeline_text")))
                                 )),
             tabPanel("Economic Effects", 
                      tabsetPanel(
                        tabPanel("GDP",
                                 # GDP plot
                                 fluidRow(column(12, htmlOutput("page_break2"))),
                                 fluidRow(
                                   column(8, plotlyOutput("gdp_plot", height = "575px")),
                                   column(4, htmlOutput("gdp_text"))
                                 )),
                        tabPanel("Gini",
                                 # Gini plot
                                 fluidRow(column(12, htmlOutput("page_break3"))),
                                 fluidRow(
                                   column(8, plotOutput("gini_plot"),
                                          fluidRow(column(4, 
                                                          selectInput(inputId = "gini_colonizer",
                                                                      label = "Choose Your Colonizer",
                                                                      choices = c("Belgium", "Britain", "France", "Germany", "Italy", "Netherlands", "Portugal", "Spain", "None"),
                                                                      multiple = TRUE,
                                                                      selected = "None")),
                                                   column(8,
                                                          sliderInput(inputId = "gini_year",
                                                                      label = "Choose years:",
                                                                      min = 1800, max = 2023,
                                                                      value = c(1800, 2023)))
                                          )),
                                   column(4, htmlOutput("gini_text"))
                                 )
                                 )
                      )
                      ),
             tabPanel("Global Values",
                      fluidRow(
                        column(1),
                        column(6, selectInput(inputId = "wvs_value", 
                                  label = "Choose World Values Survey Item Type",
                                  choices = c("Social Trust (People)", "Social Trust (Institutions)", "Economic Values", "Non-voting Participation", "Voter Participation", "Election Fairness", "Perceptions of Democracy", "Satisfaction with System", "Nationalism"))
                                  )),
                      fluidRow(
                        column(1),
                        column(6, plotlyOutput("wvs_plot", height = "500px")),
                        column(4, uiOutput("wvs_text")),  
                        column(1)
                      )
                      ),
             tabPanel("Constitutions",
                      fluidRow(
                        column(8,
                               plotOutput("cons_plot", height = "550px"),
                               sliderInput(inputId = "cons_year",
                                           label = "Choose year:",
                                           min = 1789, max = 2023,
                                           value = 1789,
                                           step = 1,
                                           animate = list(interval = 250)) 
                        ),
                        column(4, uiOutput("cons_text"))
                      )
                      ), 
             tabPanel("Behind the Scenes", includeMarkdown("final_paper.md"))
  )
)