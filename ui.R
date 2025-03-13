#loading packages

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggpubr)


ui <- fluidPage(
  titlePanel("Eduvos IT Graduate Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:",
                  choices = c("Programming Languages", "Databases", "Platforms", "Web Frameworks", "AI Search Tools", "AI Developer Tools")),
      selectInput("study_field", "Select Study Field:",
                  choices = unique(grad.survey$StudyField),
                  selected = unique(grad.survey$StudyField)[1],
                  multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tools Usage", plotOutput("toolsPlot")),
        tabPanel("Industries", plotOutput("industryPlot")),
        tabPanel("Job Roles", plotOutput("rolesPlot")),
        tabPanel("Employment Rate", plotOutput("employmentPlot"))
      )
    )
  )
)
