library(tidyverse)
library(shinydashboard)
library(elo)
library(shiny)

# Load the datasets to be used in this project
elo_df <- read.csv("UFC-ELO-Dashboard/elo_df.csv")
df <- read.csv("UFC-ELO-Dashboard/elo.csv")
about_elo <- readLines("UFC-ELO-Dashboard/ELO.txt")

# Update the datasets by dropping the index colun X
# created when writing to csv
elo_df <- elo_df %>% select(-X)
df <- df %>% select(-X)

# define weight classes vector to be used in selecting weight classes
w_classes <- elo_df %>%
  select(weight_class) %>%
  distinct() %>%
  arrange(weight_class)
# define a function to create elo data with the parameter k as the main input
create_elo_data <- function(k, data = elo_df){
  temp_df <- elo.run(winner ~ fighter + opponent , k = k,
                     data = data %>% arrange(fighter, date)) %>%
    as_tibble() %>%
    cbind(elo_df %>% arrange(fighter, date) %>% select(match_id)) %>%
    select(team.A, team.B, elo.A, elo.B, match_id)
  
  rbind(temp_df %>% select_at(vars(contains(".A"), contains("match_id"))) %>%
          rename_all(.funs = function(x) str_replace(x, ".A", "")),
        temp_df %>% select_at(vars(contains(".B"), contains("match_id"))) %>%
          rename_all(.funs = function(x) str_replace(x, ".B", ""))) %>%
    rename("fighter" = "team") %>%
    left_join(df %>% select(fighter, date, weight_class, match_id),
              by = c("fighter", "match_id")) %>%
    mutate(date = as.Date(date))
}

# define a function to get the top 5 fighters from data developed by the 
# `create_elo_data()` function
get_top_5 <- function(df, var1, var2) {
  df %>%
    group_by({{var1}}) %>%
    arrange(desc({{var2}})) %>%
    slice(1) %>%
    ungroup() %>%
    top_n({{var2}}, n = 5) %>%
    arrange(desc({{var2}})) %>%
    mutate(rank = row_number()) %>%
    select({{var1}}, {{var2}}, rank)
}

# create user interface using the `dashboardPage()` to leverage on an already implemented
# dashboard canvas that allows easy additions to it to make work easier 
ui <- dashboardPage(
  dashboardHeader(title = "UFC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About ELO",
               tabName = "about_elo_tab",
               icon = icon("code")),
      menuItem("Top 5",
               tabName = "top_5_contenders",
               icon = icon("crown")),
      menuItem("Weight Class",
               tabName = "weight_class_tab",
               icon = icon("chart-simple")),
      menuItem("Head to head",
               tabName = "head_tab",
               icon = icon("fire"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about_elo_tab",
              textOutput("about_elo")),
      tabItem(tabName = "top_5_contenders",
              fluidRow(box(selectInput("w_class_input_0", "weight class",
                                       choices = w_classes)),
                       box(sliderInput(inputId = "k_0",
                                       label = "K for ELO",
                                       min = 1,
                                       max = 100,
                                       value = 20))),
              box(tableOutput("top_5"))
              ),
      tabItem(tabName = "weight_class_tab",
              fluidRow(box(selectInput("weight_class_input", "weight class",
                                       choices = w_classes)),
                       box(sliderInput(inputId = "k",
                                       label = "K for ELO",
                                       min = 1,
                                       max = 100,
                                       value = 20))
                       ),
              box(plotOutput("elo_ts")),
              box(plotOutput("elo_dist"))
              ),
      tabItem(tabName = "head_tab",
              fluidRow(box(uiOutput("fighter_selector")), 
                       box(uiOutput("opponent_selector"))),
              fluidRow(box(valueBoxOutput("fighter_card")),
                       box(valueBoxOutput("opponent_card"))),
              box(selectInput("w_class_input_1", "weight class",
                              choices = w_classes)),
              box(sliderInput(inputId = "k_2",
                              label = "K for ELO",
                              min = 1,
                              max = 100,
                              value = 20))
              )
    )
  )
)


server <- function(input, output, session) {
  # render the details about ELO on the first menu item
  output$about_elo <- renderText({
    about_elo
  })
  
  # render the table of the top 5 fighters as per different values of k
  output$top_5 <- renderTable({
    table_df <- create_elo_data(input$k_0) %>%
      filter(weight_class == input$w_class_input_0)
    
    get_top_5(table_df, fighter, elo)
  })
  
  output$top_5_table <- renderTable({
    table_df <- create_elo_data(input$k) %>%
      filter(weight_class == input$weight_class_input)
    
    get_top_5(table_df, fighter, elo)
    })
  
  # render a time series plot while indicating the position of the top 5 fighter 
  # taking changes in k into account
  output$elo_ts <- renderPlot({
    elo_ts_df <- create_elo_data(input$k) %>%
      filter(weight_class == input$weight_class_input)
    
    top_5_fighters <- get_top_5(elo_ts_df, fighter, elo) %>% select(fighter)
    
    # the plot
    ggplot(data = elo_ts_df, aes(x = date, y = elo)) + 
      geom_point() +
      geom_point(data = elo_ts_df %>% filter(fighter %in% top_5_fighters$fighter),
                 aes(x = date, y = elo, color = fighter)) +
      theme(legend.position = "top")
  })
  
  # render a fighter distribution plot that accounts for different weight class
  # elos
  output$elo_dist <- renderPlot({
    elo_dst <- create_elo_data(input$k) %>%
      filter(weight_class == input$weight_class_input)
    
    ggplot(data = elo_dst, aes(x = elo)) + geom_histogram()
  })
  
  # create a selector for both the fighter and the opponent
  output$fighter_selector <- renderUI({
    fighter_selector_df <- create_elo_data(input$k_2) %>%
      filter(weight_class == input$w_class_input_1) %>%
      select(fighter) %>%
      distinct() %>%
      arrange(fighter)
    
    selectInput(inputId = "v_fighter",
                label = "Fighter",
                choices = fighter_selector_df)
  })
  output$opponent_selector <- renderUI({
    opponent_selector_df <- create_elo_data(input$k_2) %>%
      filter(weight_class == input$w_class_input_1 & 
               fighter != input$v_fighter) %>%
      select(fighter) %>%
      distinct() %>%
      arrange(fighter)
    
    selectInput(inputId = "v_opponent",
                label = "Opponent",
                choices = opponent_selector_df)
  })
  
  output$fighter_card <- renderValueBox({
    elo <- elo.run(winner ~ fighter + opponent,
                   k = input$k_2,
                   data = elo_df)
    
    fighter_prob <- round(100 * predict(elo, data.frame(fighter = input$v_fighter,
                                            opponent = input$v_opponent)),
                          0)
    
    valueBox(
      value = paste(fighter_prob, "%", sep = ""),
      subtitle = paste(input$v_fighter, "Probability", " "),
      color = "blue",
      icon = icon("hand-rock")
    )
  })
  output$opponent_card <- renderValueBox({
    elo <- elo.run(winner ~ fighter + opponent,
                   k = input$k_2,
                   data = elo_df)
    
    opponent_prob <- round(100 * predict(elo, data.frame(fighter = input$v_opponent,
                                                        opponent = input$v_fighter)),
                          0)
    
    valueBox(
      value = paste(opponent_prob, "%", sep = ""),
      subtitle = paste(input$v_opponent, "Probability", " "),
      color = "red",
      icon = icon("hand-rock")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


