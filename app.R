library(shiny)
library(bslib)
library(ggplot2)
library(tidyr)
library(dplyr)
library(uuid)

# Function to save responses (works differently in browser vs server)
saveResponses <- function(data, user_id, timestamp) {
  if (Sys.getenv("SHINYLIVE") == "1") {
    # Browser storage using localStorage
    jsonlite::write_json(
      list(
        data = data,
        user_id = user_id,
        timestamp = timestamp
      ),
      "responses.json"
    )
  } else {
    # Server storage
    data$user_id <- user_id
    data$timestamp <- timestamp
    
    if (file.exists("app/results/responses.csv")) {
      write.csv(data, "app/results/responses.csv", append = TRUE, row.names = FALSE)
    } else {
      write.csv(data, "app/results/responses.csv", row.names = FALSE)
    }
  }
}

ui <- page_fillable(
  theme = bs_theme(version = 5),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  div(
    style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
    card(
      width = "300px",
      navset_card_tab(
        id = "wizard",
        nav_panel(
          title = "Home",
          value = "page1",
          div(
            class = "text-center p-3",
            h3("Welcome to our Survey"),
            p("Please click Start to begin"),
            actionButton("start", "Start Survey", class = "btn-lg btn-primary w-100")
          )
        ),
        nav_panel(
          title = "Personal Info",
          value = "page2",
          div(
            class = "p-3",
            textInput("name", "First Name*"),
            textInput("surname", "Last Name*"),
            numericInput("age", "Age*", value = NA),
            selectInput("gender", "Gender*", 
                        choices = c("", "Male", "Female", "Non-binary", "Prefer not to say")),
            actionButton("next1", "Next", class = "btn-primary w-100 mt-3")
          )
        ),
        nav_panel(
          title = "Political View",
          value = "page3",
          div(
            class = "p-3",
            sliderInput("position", "Political Position (Left to Right)*",
                        min = 0, max = 10, value = 5, step = 1),
            sliderInput("ideology", "Political Ideology (Liberal to Conservative)*",
                        min = 0, max = 10, value = 5, step = 1),
            actionButton("submit", "Submit", class = "btn-success w-100 mt-3")
          )
        ),
        nav_panel(
          title = "Results",
          value = "page4",
          div(
            class = "p-3",
            h4("Your Responses"),
            tableOutput("results"),
            br(),
            h4("Your Political Position"),
            plotOutput("scatterplot", height = "300px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Generate unique ID for session
  user_id <- reactive({
    UUIDgenerate()
  })
  
  responses <- reactiveVal(NULL)
  
  observeEvent(input$start, {
    updateTabsetPanel(session, "wizard", selected = "page2")
  })
  
  observeEvent(input$next1, {
    req(input$name, input$surname, input$age, input$gender)
    if (input$name == "" || input$surname == "" || is.na(input$age) || input$gender == "") {
      showNotification("Please fill in all fields", type = "error")
    } else {
      updateTabsetPanel(session, "wizard", selected = "page3")
    }
  })
  
  observeEvent(input$submit, {
    current_time <- Sys.time()
    
    response_data <- data.frame(
      Name = input$name,
      Surname = input$surname,
      Age = input$age,
      Gender = input$gender,
      LeftRight = input$position,
      LibCons = input$ideology
    )
    
    # Save responses
    saveResponses(response_data, user_id(), current_time)
    responses(response_data)
    
    updateTabsetPanel(session, "wizard", selected = "page4")
  })
  
  output$results <- renderTable({
    req(responses())
    df <- responses()
    df[] <- lapply(df, as.character)
    
    long_df <- pivot_longer(df, everything(), 
                            names_to = "Variable", 
                            values_to = "Value")
    
    long_df$Variable <- factor(long_df$Variable,
                               levels = c("Name", "Surname", "Age", "Gender", "LeftRight", "LibCons"),
                               labels = c("First Name", "Last Name", "Age", "Gender", 
                                          "Left-Right Position", "Liberal-Conservative Position")
    )
    long_df
  })
  
  output$scatterplot <- renderPlot({
    req(responses())
    
    ggplot(responses(), aes(x = LeftRight, y = LibCons)) +
      geom_point(size = 5, color = "blue") +
      labs(x = "Left (0) to Right (10)",
           y = "Liberal (0) to Conservative (10)") +
      theme_minimal() +
      coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
      theme(text = element_text(size = 12))
  })
}

shinyApp(ui, server)