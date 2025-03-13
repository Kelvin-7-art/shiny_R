library(dplyr)

grad.survey <- read.csv(file.choose())

head(grad.survey)

# Selecting relevant columns
grad.survey <- grad.survey %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, WebFramework, Industry, AISearch, AITool, Employment)

# Check for missing values
colSums(is.na(grad.survey))
grad.survey <- grad.survey %>%
  filter(!is.na(Campus), !is.na(StudyField))
grad.survey <- grad.survey %>%
  mutate(across(c(ProgLang, Databases, Platform, WebFramework, AISearch, AITool), ~replace_na(., "Unknown")))

# Standardizing Categorical ColumnsStandardize similar categories:

grad.survey <- grad.survey %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban", "Umhlanga") ~ "Durban",
    Campus == "Midrand" ~ "Midrand",
    Campus == "Cape Town" ~ "Cape Town",
    TRUE ~ Campus
  ))
# Find top 5 campuses with the most responses
top_campuses <- grad.survey %>%
  group_by(Campus) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  pull(Campus)

# Filter dataset to only include top campuses
grad.survey <- grad.survey %>%
  filter(Campus %in% top_campuses)


library(ggplot2)
library(dplyr)
library(ggpubr)
install.packages("ggpubr")


# 1. Campus Distribution (Bar Plot)
ggplot(grad.survey, aes(x = Campus, fill = Campus)) +
  geom_bar() +
  labs(title = "Number of Graduates per Campus",
       x = "Campus",
       y = "Count") +
  theme_minimal()

# 2. Study Field Distribution (Pie Chart)
study_field <- grad.survey %>%
  group_by(StudyField) %>%
  summarise(Count = n())

ggplot(study_field, aes(x = "", y = Count, fill = StudyField)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Study Fields") +
  theme_void()

# 3. Employment Status (Bar Plot)
ggplot(grad.survey, aes(x = Employment, fill = Employment)) +
  geom_bar() +
  labs(title = "Employment Status of Graduates",
       x = "Employment",
       y = "Count") +
  theme_minimal()

# 4. Most Popular Programming Languages
prog_lang <- grad.survey %>%
  group_by(ProgLang) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)

ggplot(prog_lang, aes(x = reorder(ProgLang, Count), y = Count, fill = ProgLang)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Programming Languages",
       x = "Programming Languages",
       y = "Count") +
  theme_minimal()

# 5. Web Framework Usage
web_framework <- grad.survey %>%
  group_by(WebFramework) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(5)

ggplot(web_framework, aes(x = reorder(WebFramework, Count), y = Count, fill = WebFramework)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 Web Frameworks",
       x = "Web Frameworks",
       y = "Count") +
  theme_minimal()

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(ggpubr)


library(shiny)
install.packages("DT")
library(ggplot2)
library(DT)
library(dplyr)
library(ggpubr)


# Define UI
ui <- fluidPage(
  titlePanel("Quarto Shiny Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 15)
    ),
    mainPanel(
      plotOutput("campusPlot"),
      plotOutput("studyFieldPlot"),
      plotOutput("employmentPlot"),
      plotOutput("progLangPlot"),
      plotOutput("webFrameworkPlot")
    )
  )
)
