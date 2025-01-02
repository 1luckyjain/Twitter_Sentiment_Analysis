# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(readxl)
library(plotly)
library(shinythemes)
library(DT)
library(shinyjs)
library(httr)
library(fontawesome)

# Configure Google OAuth credentials
google_client_id <- "695491232934-2qj7ofvi87ggm6dmcst86q3ublnplmpj.apps.googleusercontent.com"
google_client_secret <- "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"
oauth_endpoints("google")

# Define scopes
scopes <- c(
  "https://www.googleapis.com/auth/userinfo.profile",
  "https://www.googleapis.com/auth/userinfo.email"
)
# Define login UI
loginUI <- function() {
  tagList(
    div(
      style = "display: flex; justify-content: center; align-items: center; height: 100vh; 
               background-image: url('https://static.vecteezy.com/system/resources/previews/025/120/727/non_2x/a-modern-retail-store-with-abundant-merchandise-generated-by-ai-photo.jpg');
               background-size: cover; 
               background-position: center; 
               background-repeat: no-repeat; 
               color: white; 
               font-family: 'Arial', sans-serif;",
      div(
        style = "background: rgba(0, 0, 0, 0.7); 
                 padding: 2rem; 
                 border-radius: 15px; 
                 box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2); 
                 text-align: center; 
                 max-width: 500px; 
                 width: 90%;",
        
        h1("Twitter Sentiment Analytics", 
           style = "font-size: 2.5rem; margin-bottom: 0.5rem;"),
        
        p("Login to Unlock Powerful Analytics", 
          style = "font-size: 1.2rem; margin-bottom: 1.5rem; color: #ddd; font-weight: bold;"),
        
        hr(style = "border-top: 1px solid #555; margin-bottom: 1.5rem;"),
        
        actionButton(
          inputId = "login_btn", 
          label = "Login with Google", 
          icon = icon("google"), 
          style = "background-color: #4285F4; 
                   color: white; 
                   border: none; 
                   padding: 0.8rem 1.5rem; 
                   border-radius: 5px; 
                   font-size: 1rem; 
                   font-weight: bold; 
                   cursor: pointer;"
        )
      )
    )
  )
}

# Define main UI
mainUI <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f4f4f4;
          font-family: 'Arial', sans-serif;
        }
        .home-box {
          padding: 20px; 
          text-align: center; 
          background-color: rgba(255, 255, 255, 0.9); 
          border-radius: 10px; 
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          transition: transform 0.3s;
          margin: 20px;
        }
        .home-box:hover {
          transform: scale(1.05);
        }
        .welcome-text {
          color: #2C3E50;
          font-size: 2rem;
          margin-bottom: 10px;
        }
        .description-text {
          font-size: 1.2rem;
          color: #555;
        }
        .logout-btn {
          margin-top: 20px;
          background-color: #e74c3c;
          color: white;
          border: none;
          padding: 10px 20px;
          border-radius: 5px;
          cursor: pointer;
          transition: background-color 0.3s;
        }
        .logout-btn:hover {
          background-color: #c0392b;
        }
        .feature-box {
          background-color: #ffffff;
          border-radius: 10px;
          padding: 15px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          margin: 10px;
          transition: transform 0.3s }
        .feature-box:hover {
          transform: translateY(-5px);
        }
        .icon {
          font-size: 3rem;
          color: #3498db;
        }
      "))
    ),
    navbarPage(
      title = "Twitter Analytics",
      tabPanel("Home", icon = icon("home"),
               fluidRow(
                 column(12,
                        div(class = "home-box",
                            h3(class = "welcome-text", "Welcome to Twitter Sentiment Analytics"),
                            p(class = "description-text", "Transform your Twitter data into actionable insights with our powerful analytics tools."),
                            actionButton("logout", "Logout", 
                                         class = "logout-btn")
                        )
                 )
               ),
               fluidRow(
                 column(4, div(class = "feature-box",
                               icon("chart-line", class = "icon"),
                               h4("Real-time Analytics"),
                               p("Get insights on your Twitter data as it happens.")
                 )),
                 column(4, div(class = "feature-box",
                               icon("users", class = "icon"),
                               h4("User  Engagement"),
                               p("Analyze how users interact with your content.")
                 )),
                 column(4, div(class = "feature-box",
                               icon("cogs", class = "icon"),
                               h4("Custom Reports"),
                               p("Generate reports tailored to your needs.")
                 ))
               )
      ),
      tabPanel("Analysis",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "box",
                       h4("Control Panel"),
                       selectInput("treemap_var", 
                                   "Treemap Variable:", 
                                   choices = c("Unit ID" = "X_unit_id",
                                               "Trusted Judgments" = "X_trusted_judgments")),
                       
                       selectInput("heatmap_var", 
                                   "Heatmap Variable:", 
                                   choices = c("Trusted Judgments" = "X_trusted_judgments",
                                               "Unit ID" = "X_unit_id")),
                       
                       selectInput("scatter_x", 
                                   "3D Scatter X:", 
                                   choices = c("Unit ID" = "X_unit_id",
                                               "Trusted Judgments" = "X_trusted_judgments",
                                               "Time" = "timestamp")),
                       
                       selectInput("scatter_y", 
                                   "3D Scatter Y:", 
                                   choices = c("Trusted Judgments" = "X_trusted_judgments",
                                               "Unit ID" = "X_unit_id",
                                               "Time" = "timestamp")),
                       
                       selectInput("scatter_z", 
                                   "3D Scatter Z:", 
                                   choices = c("Time" = "timestamp",
                                               "Unit ID" = "X_unit_id",
                                               "Trusted Judgments" = "X_trusted_judgments")),
                       
                       sliderInput("sample_size",
                                   "Sample Size:",
                                   min = 100,
                                   max = 1000,
                                   value = 500,
                                   step = 100),
                       
                       actionButton("refresh", "Refresh Data", 
                                    class = "btn-primary btn-block")
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   tabsetPanel(
                     tabPanel("Overview",
                              fluidRow(
                                column(6, 
                                       div(class = "box",
                                           h4("Data Summary"),
                                           verbatimTextOutput("summary")
                                       )
                                ),
                                column(6, 
                                       div(class = "box",
                                           h4("Sample Data"),
                                           DTOutput("sample_table")
                                       )
                                )
                              )
                     ),
                     
                     tabPanel("Visualizations",
                              fluidRow(
                                column(6, 
                                       div(class = "box",
                                           h4("Treemap Visualization"),
                                           plotOutput("treemapPlot")
                                       )
                                ),
                                column(6, 
                                       div(class = "box",
                                           h4("Heatmap Visualization"),
                                           plotlyOutput("heatmapPlot")
                                       )
                                )
                              ),
                              fluidRow(
                                column(12, 
                                       div(class = "box",
                                           h4("3D Scatter Plot"),
                                           plotlyOutput("scatterPlot3D", height = "600px")
                                       )
                                )
                              )
                     )
                   )
                 )
               )
      )
    )
  )
}

# Define UI
ui <- function(request) {
  tagList(
    useShinyjs(),
    uiOutput("page")
  )
}

# Define server
server <- function(input, output, session) {
  
  # Authentication status and user info
  auth_status <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  
  # Handle Google login
  observeEvent(input$login_btn, {
    tryCatch({
      # Create OAuth app
      myapp <- oauth_app("google", key = google_client_id, secret = google_client_secret)
      
      # Get token
      goog_token <- oauth2.0_token(oauth_endpoints("google"), myapp, 
                                    scope = scopes, cache = FALSE)
      
      # Get user information
      user_info_response <- GET("https://www.googleapis.com/oauth2/v2/userinfo", 
                                config(token = goog_token))
      user_data <- content(user_info_response)
      
      # Update reactive values
      user_info(user_data)
      auth_status(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Login failed:", e$message), type = "error")
    })
  })
  
  # Render appropriate page based on authentication
  output$page <- renderUI({
    if (!auth_status()) {
      loginUI()
    } else {
      mainUI()
    }
  })
  
  # Display user welcome message
  output$user_welcome <- renderText({
    req(user_info())
    paste("Welcome,", user_info()$given_name, "!")
  })
  
  # Handle logout
  observeEvent(input$logout, {
    auth_status(FALSE)
    user_info(NULL)
    session$reload()
  })
  
  # Your existing reactive data handling
  data <- reactiveVal()
  
  # Main application logic
  observe({
    req(auth_status())  # Only run when authenticated
    
    # File upload handling
    observeEvent(input$file, {
      tryCatch({
        df <- read_excel(input$file$datapath)
        required_cols <- c("Sales", "Profit", "Discount", "State", "Category", "Sub -Category")
        
        if (!all(required_cols %in% colnames(df))) {
          stop("Missing required columns.")
        }
        
        if (!is.numeric(df$Sales) || !is.numeric(df$Profit) || !is.numeric(df$Discount)) {
          stop("Sales, Profit, and Discount must be numeric.")
        }
        
        data(df)
        output$uploadStatus <- renderUI({
          div(style = "color: green;", "File uploaded successfully!")
        })
        shinyjs::enable("analyzeBtn")
        
      }, error = function(e) {
        output$uploadStatus <- renderUI({
          div(style = "color: red;", paste("Error:", e$message))
        })
        shinyjs::disable("analyzeBtn")
      })
    })
    
    # Stats Boxes
    output$statsBoxes <- renderUI({
      req(data())
      fluidRow(
        column(3, div(class = "stats-box", p(length(unique(data()$State)), "States"))),
        column(3, div(class = "stats-box", p(length(unique(data()$Category)), "Categories"))),
        column(3, div(class = "stats-box", p(length(unique(data()$`Sub-Category`)), "Sub-Categories"))),
        column(3, div(class = "stats-box", p(nrow(data()), "Total Records")))
      )
    })
    
    # Filtered Data
    filteredData <- reactive({
      req(data(), input$salesRange)
      data() %>% filter(Sales >= input$salesRange[1], Sales <= input$salesRange[2])
    })
    
    # DataTable
    output$data_table <- DT::renderDataTable({
      req(filteredData())
      DT::datatable(filteredData(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Plot
    output$plot <- renderPlotly({
      req(filteredData(), input$xvar, input$yvar)
      plot <- ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(aes(color = ..y..), size = 4, alpha = 0.8) +
        scale_color_gradient(low = "#4ECDC4", high = "#FF7F50") +
        theme_minimal()
      ggplotly(plot)
    })
    
    # Model
    observeEvent(input$run_model, {
      req(data())
      df_ml <- data() %>% select(Sales, Discount, Profit)
      model <- randomForest(Profit ~ Sales + Discount, data = df_ml, ntree = 100)
      predicted_profit <- predict(model, newdata = df_ml)
      
      output$model_result <- renderPrint({
        cat("First 10 Predicted Profit Values:\n")
        print(head(predicted_profit, 10))
      })
    })
    
    # Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("filtered-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filteredData(), file, row.names = FALSE)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)