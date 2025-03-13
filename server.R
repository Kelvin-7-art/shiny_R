server <- function(input, output) {
  output$toolsPlot <- renderPlot({
    data <- switch(input$category,
                   "Programming Languages" = split_and_count(grad.survey, "ProgLang"),
                   "Databases" = split_and_count(grad.survey, "Databases"),
                   "Platforms" = split_and_count(grad.survey, "Platform"),
                   "Web Frameworks" = split_and_count(grad.survey, "WebFramework"),
                   "AI Search Tools" = split_and_count(grad.survey, "AISearch"),
                   "AI Developer Tools" = split_and_count(grad.survey, "AITool"))
    ggplot(data, aes(x = reorder(get(input$category), Count), y = Count, fill = get(input$category))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top", input$category), x = input$category, y = "Count") +
      theme_minimal()
  })
  
  output$industryPlot <- renderPlot({
    industry_data <- grad.survey %>% 
      filter(StudyField %in% input$study_field) %>%
      separate_rows(Industry, sep = ";") %>%
      group_by(StudyField, Industry) %>% 
      summarise(Count = n(), .groups = 'drop') %>% 
      arrange(desc(Count))
    ggplot(industry_data, aes(x = reorder(Industry, Count), y = Count, fill = StudyField)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Popular Industries by Study Field", x = "Industry", y = "Count") +
      theme_minimal()
  })
  
  output$rolesPlot <- renderPlot({
    job_roles <- grad.survey %>% 
      filter(StudyField %in% input$study_field) %>%
      group_by(StudyField, Role) %>% 
      summarise(Count = n(), .groups = 'drop') %>% 
      arrange(desc(Count)) %>%
      group_by(StudyField) %>%
      slice_max(Count, n = 1)
    ggplot(job_roles, aes(x = reorder(Role, Count), y = Count, fill = StudyField)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count") +
      theme_minimal()
  })
  
  output$employmentPlot <- renderPlot({
    employment_rate <- grad.survey %>% 
      filter(StudyField %in% input$study_field) %>%
      group_by(StudyField, Employment) %>% 
      summarise(Count = n(), .groups = 'drop') %>% 
      mutate(Percentage = (Count / sum(Count)) * 100) %>% 
      filter(Employment == "Employed")
    ggplot(employment_rate, aes(x = reorder(StudyField, Percentage), y = Percentage, fill = StudyField)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)