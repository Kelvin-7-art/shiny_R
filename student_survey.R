# ------------------------------ QUESTION 1
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
# qUESTION 2
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggpubr)

# Splitting multi-response columns
split_and_count <- function(df, column) {
  df %>% 
    separate_rows(!!sym(column), sep = ";") %>% 
    group_by(!!sym(column)) %>% 
    summarise(Count = n()) %>% 
    filter(!!sym(column) != "Unknown" & !!sym(column) != "") %>% 
    arrange(desc(Count)) %>% 
    head(10)
}

# Top Programming Languages
prog_lang <- split_and_count(grad.survey, "ProgLang")
ggplot(prog_lang, aes(x = reorder(ProgLang, Count), y = Count, fill = ProgLang)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Programming Languages", x = "Programming Language", y = "Count") +
  theme_minimal()

# Top Databases
databases <- split_and_count(grad.survey, "Databases")
ggplot(databases, aes(x = reorder(Databases, Count), y = Count, fill = Databases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Databases", x = "Database", y = "Count") +
  theme_minimal()

# Top Platforms
platforms <- split_and_count(grad.survey, "Platform")
ggplot(platforms, aes(x = reorder(Platform, Count), y = Count, fill = Platform)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Platforms", x = "Platform", y = "Count") +
  theme_minimal()

# Top Web Frameworks
web_framework <- split_and_count(grad.survey, "WebFramework")
ggplot(web_framework, aes(x = reorder(WebFramework, Count), y = Count, fill = WebFramework)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Web Frameworks", x = "Web Framework", y = "Count") +
  theme_minimal()

# Top AI Search Tools
ai_search <- split_and_count(grad.survey, "AISearch")
ggplot(ai_search, aes(x = reorder(AISearch, Count), y = Count, fill = AISearch)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top AI Search Tools", x = "AI Search Tool", y = "Count") +
  theme_minimal()

# Top AI Developer Tools
ai_tool <- split_and_count(grad.survey, "AITool")
ggplot(ai_tool, aes(x = reorder(AITool, Count), y = Count, fill = AITool)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top AI Developer Tools", x = "AI Developer Tool", y = "Count") +
  theme_minimal()

# Industries by Study Field
industries <- grad.survey %>% 
  separate_rows(Industry, sep = ";") %>%
  group_by(StudyField, Industry) %>% 
  summarise(Count = n(), .groups = 'drop') %>% 
  arrange(desc(Count))
ggplot(industries, aes(x = reorder(Industry, Count), y = Count, fill = StudyField)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Popular Industries by Study Field", x = "Industry", y = "Count") +
  theme_minimal()

# Top Job Roles by Study Field
job_roles <- grad.survey %>% 
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

# Employment Rate by Study Field
employment_rate <- grad.survey %>% 
  group_by(StudyField, Employment) %>% 
  summarise(Count = n(), .groups = 'drop') %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(Employment == "Employed")
ggplot(employment_rate, aes(x = reorder(StudyField, Percentage), y = Percentage, fill = StudyField)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
  theme_minimal()


# -------------- END OF QUESTION 2