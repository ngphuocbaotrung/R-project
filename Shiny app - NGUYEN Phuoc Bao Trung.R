# Load necessary libraries
install.packages("shiny")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
library(shiny)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(stringr)

# Load the Enron data
rm(list = ls())
setwd("C:/Users/TRUNG NGUYEN/Downloads")
load("Enron (1).Rdata")

# Data preprocessing
employeelist <- employeelist %>%
  mutate(status = recode(status, "N/A" = "Unknown")) %>%
  mutate(status = replace_na(status, "Unknown"))

# Reshape the employeelist (gather emails into a single column)
employee_long <- employeelist %>%
  pivot_longer(cols = starts_with("Email"), 
               names_to = "EmailType", 
               values_to = "Email") %>%
  filter(!is.na(Email) & Email != "")

# Join the employee table with the message table
merged_table_employeemessage <- left_join(
  employee_long, 
  message, 
  by = c("Email" = "sender")
)

# Create a table with all information
emp_mess_table_final <- merged_table_employeemessage %>%
  select(firstName, lastName, status, Email, mid, date, message_id, subject) %>% 
  distinct()

emp_mess_table_final <- emp_mess_table_final %>%
  mutate(fullname = paste(lastName, firstName, sep = " "))

# Clean emails from 1999 - 2002
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2002-12-31")
emp_mess_table_final <- emp_mess_table_final %>%
  filter(date >= start_date & date <= end_date)

# Define UI
ui <- fluidPage(
  titlePanel("Enron Email Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      # Input for Top Active Employees
      numericInput("num_employees", "Number of Top Active Employees:", 
                   value = 5, min = 1, max = 50),
      # Input for Role Analysis
      checkboxGroupInput("selected_roles", "Select Roles:", 
                         choices = unique(emp_mess_table_final$status),
                         selected = unique(emp_mess_table_final$status)),
      # Input for Date Range in Temporal Analysis
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(emp_mess_table_final$date),
                     end = max(emp_mess_table_final$date)),
      # Input for Number of Top Words in Content Analysis
      numericInput("num_words", "Number of Top Words:", 
                   value = 10, min = 5, max = 50)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Active Employees",
                 h3("Top Active Employees"),
                 tableOutput("activeEmployeesTable"),
                 plotOutput("activeEmployeesPlot")),
        tabPanel("Role Analysis",
                 h3("Emails Sent by Role"),
                 tableOutput("emailsByRoleTable"),
                 plotOutput("emailsByRolePlot"),
                 h3("Number of Employees by Role"),
                 tableOutput("employeesCountTable"),
                 plotOutput("employeesCountPlot")
        ),
        tabPanel("Temporal Analysis",
                 h3("Email Activity Over Time"),
                 plotOutput("emailsOverTimePlot"),
                 h3("Event Overlay"),
                 plotOutput("eventOverlayPlot")),
        tabPanel("Content Analysis",
                 h3("Top Words in Email Subjects"),
                 tableOutput("topWordsTable"),
                 plotOutput("topWordsPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  ### Active Employees Analysis ###
  active_employees <- reactive({
    emp_mess_table_final %>%
      group_by(fullname,status) %>%            
      summarise(Email_count = n()) %>% 
      arrange(desc(Email_count)) %>%
      head(input$num_employees)
  })
  
  output$activeEmployeesTable <- renderTable({
    active_employees()
  })
  
  output$activeEmployeesPlot <- renderPlot({
    ggplot(active_employees(), aes(x = reorder(fullname, Email_count), y = Email_count)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Active Employees",
           x = "Employee",
           y = "Number of Emails Sent") +
      theme_minimal()
  })
  
  ### Role Analysis ###
  # Emails sent by role
  emails_by_role <- reactive({
    emp_mess_table_final %>%
      filter(status %in% input$selected_roles) %>%
      group_by(status) %>%
      summarise(Emails_Sent = n()) %>%
      arrange(desc(Emails_Sent))
  })
  
  output$emailsByRoleTable <- renderTable({
    emails_by_role()
  })
  
  output$emailsByRolePlot <- renderPlot({
    ggplot(emails_by_role(), aes(x = reorder(status, Emails_Sent), y = Emails_Sent)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(title = "Emails Sent by Role",
           x = "Role",
           y = "Number of Emails Sent") +
      theme_minimal()
  })
  
  # Employee counts by role
  employees_by_role <- reactive({
    employeelist %>%
      group_by(status) %>%
      summarize(Employee_Count = n()) %>%
      ungroup()
  })
  
  output$employeesCountTable <- renderTable({
    employees_by_role()
  })
  
  output$employeesCountPlot <- renderPlot({
    ggplot(employees_by_role(), aes(x = reorder(status, Employee_Count), y = Employee_Count)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Number of Employees by Role",
           x = "Role",
           y = "Number of Employees") +
      theme_minimal()
  })
  
  ### Temporal Analysis ###
  filtered_data <- reactive({
    emp_mess_table_final %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  output$emailsOverTimePlot <- renderPlot({
    emails_by_day <- filtered_data() %>%
      group_by(date) %>%
      summarise(emails_sent = n())
    
    ggplot(emails_by_day, aes(x = date, y = emails_sent)) +
      geom_line(color = "blue") +
      labs(title = "Emails Sent Over Time",
           x = "Date",
           y = "Number of Emails Sent") +
      theme_minimal()
  })
  
  output$eventOverlayPlot <- renderPlot({
    emails_by_day <- filtered_data() %>%
      group_by(date) %>%
      summarise(emails_sent = n())
    
    events <- data.frame(
      date = as.Date(c("1999-11-29", "2001-08-14", "2001-10-16", "2001-10-22", "2001-11-08", "2001-12-02")),
      event = c("EnronOnline Launch", "CEO Resigns", "Announces Loss", "SEC Inquiry Begins", "Earnings Restated", "Bankruptcy Filed")
    )
    
    ggplot() +
      geom_line(data = emails_by_day, aes(x = date, y = emails_sent), color = "blue") +
      geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "red") +
      geom_text(data = events, aes(x = date, y = max(emails_by_day$emails_sent), label = event),
                angle = 90, vjust = -0.5, hjust = 0, size = 3, color = "red") +
      labs(title = "Emails Over Time with Events",
           x = "Date",
           y = "Number of Emails Sent") +
      theme_minimal()
  })
  
  ### Content Analysis ###
  top_words <- reactive({
    subject_words <- emp_mess_table_final %>%
      unnest_tokens(word, subject)
    
    subject_words <- subject_words %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      head(input$num_words)
  })
  
  output$topWordsTable <- renderTable({
    top_words()
  })
  
  output$topWordsPlot <- renderPlot({
    ggplot(top_words(), aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(title = "Top Words in Email Subjects",
           x = "Word",
           y = "Frequency") +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

