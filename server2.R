# Define Server
server <- function(input, output) {
  output$campusPlot <- renderPlot({
    ggplot(grad.survey, aes(x = Campus, fill = Campus)) +
      geom_bar() +
      labs(title = "Number of Graduates per Campus", x = "Campus", y = "Count") +
      theme_minimal()
  })
  
  output$studyFieldPlot <- renderPlot({
    study_field <- grad.survey %>%
      group_by(StudyField) %>%
      summarise(Count = n())
    ggplot(study_field, aes(x = "", y = Count, fill = StudyField)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Distribution of Study Fields") +
      theme_void()
  })
  
  output$employmentPlot <- renderPlot({
    ggplot(grad.survey, aes(x = Employment, fill = Employment)) +
      geom_bar() +
      labs(title = "Employment Status of Graduates", x = "Employment", y = "Count") +
      theme_minimal()
  })
  
  output$progLangPlot <- renderPlot({
    prog_lang <- grad.survey %>%
      group_by(ProgLang) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    ggplot(prog_lang, aes(x = reorder(ProgLang, Count), y = Count, fill = ProgLang)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Programming Languages", x = "Programming Languages", y = "Count") +
      theme_minimal()
  })
  
  output$webFrameworkPlot <- renderPlot({
    web_framework <- grad.survey %>%
      group_by(WebFramework) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(5)
    ggplot(web_framework, aes(x = reorder(WebFramework, Count), y = Count, fill = WebFramework)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 5 Web Frameworks", x = "Web Frameworks", y = "Count") +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)

