# install_github("shiny", "rstudio")
# install.packages("devtools")
#install("shinyBS")
#install.packages("shinythemes")
library(DT)
library(readxl)
library(shinydashboard)
library(shiny)
library(class)
library(devtools)
library(shinyIncubator)
library(shinyBS)
library(shinythemes)

#####################################Loading Datasets################################################################

Coke <- read_excel("data/Mydata.rds", sheet = "Coke")
Gatorade <- read_excel("data/Mydata.rds", sheet = "Gatorade")  
Pureleaf <- read_excel("data/Mydata.rds", sheet = "Pureleaf")
Gatorade_test <-
  read_excel("data/Mydata.rds", sheet = "Gatorade_test")
Gatorade_training <-
  read_excel("data/Mydata.rds", sheet = "Gatorade_training")
Gatorade_userdata <-
  read_excel("data/Mydata.rds", sheet = "Gatorade_userdata")
df <- data.frame(matrix(c("0", "0"), 1, 2))
colnames(df) <- c("Count", "Weight")

###############################################UI DESIGN###############################################################

ui <- fluidPage(
  #' theme = 'cerulean',
  #' tags$head(
  #'   tags$style(HTML("
  #'                   @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
  #' 
  #'                   h1 {
  #'                   font-family: 'Lobster', cursive;
  #'                   font-weight: 500;
  #'                   line-height: 1.1;
  #'                   color: #48ca3b;
  #'                   }
  #' 
  #'                   "))))
  dashboardPage(
    dashboardHeader(title = "SmartShelf App"),
    dashboardSidebar(sidebarMenu(
      id = "tabs",
      menuItem(
        strong("Summary Statistics"),
        tabName = "statistics",
        icon = icon("th")
      ),
      
      menuItem(
        strong("Plots"),
        tabName = "plots",
        icon = icon("bar-chart-o")
      ),
      
      menuItem(
        strong("Model Summary"),
        tabName = "model",
        icon = icon("calendar")
      ),
      menuItem(
        strong("New Item Prediction"),
        tabName = "item",
        icon = icon("users")
      )
      
      
    )),
    
    dashboardBody(shinyUI(
      fluidPage(
        theme = "bootstrap.css",
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
        ),
        
        tabItems(
          tabItem(tabName = "statistics", fluidRow(
            box(collapsible = TRUE,
                verbatimTextOutput("summary"))
          )),
          
          
          tabItem(tabName = "model", fluidRow(
            box(collapsible = TRUE,
                verbatimTextOutput("model"))
          )),
          
          tabItem(tabName = "plots", fluidRow(
            collapsible = TRUE,
            box(
              title = "Scatter Plots",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("scatter", height = 250)
            ),
            
            box(
              
              title = "Histograms",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("distPlot_dv")
            ),
            
            box(
              title = "Histograms",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("distPlot_iv")
            ),
            
            box(
              title = "Residual Plot",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("residuals_scatter")
            )
            
          )),
          tabItem(tabName = "item", fluidRow(
            box(collapsible = TRUE,
                textInput("caption", "1 . Enter the name of the item: -", "Coke"),
                print(strong("2. Enter the training dataset")),
                matrixInput(
                  inputId = 'i1data',
                  label = 'Count                           Weight',
                  data = df
                ),
                fluidRow( 
                  column(width = 6,
                         wellPanel(
                           h4("Training dataset: -")
                           ,
                           tableOutput(outputId = 'newitem_training_data')
                         ))),
                print(strong("3. Enter the dataset on which the training data has to be tested.")),
                helpText("Please note. This dataset needs to be saved on the server for future use and predictions"),
                matrixInput(
                  inputId = 'i2data',
                  label = 'Count                           Weight',
                  data = df
                ),
                fluidRow( 
                  column(width = 6,
                         wellPanel(
                           h4("Test Dataset: -")
                           ,
                           tableOutput(outputId = 'newitem_test_data')
                           
                         ))),
                br(),
                numericInput(
                  "weight",
                  label = (strong("4. Enter the total Weight on scales")),
                  0.00,
                  min = 0.00,
                  max = 15.00,
                  step = 0.01,
                  width = NULL
                ),
                h3('The predicted count is:'),
                verbatimTextOutput("item")),
            tags$head(tags$style("#item {color: red;
                                 font-size: 24px;
                                 font-weight: bold;
                                 font-style: italic;
                                 }"
            )
            )
            ))
          
            ),
        sidebarLayout(
          sidebarPanel(
            h3("Problem Statement"),
            p(
              strong(
                "Prediction of count of products by measuring the total weight kept on the scales"
              )
            ),
            br(),
            img(
              src = "https://vensi.github.io/ba-demos/smartfridge/app-icon.png",
              height = 200,
              width = 200
            ),
            br(),
            "SmartShelf App is a product of ",
            span("Vensi,Inc.", style = "color:blue")
          ),
          
          
          
          mainPanel(
            tags$style(
              type = 'text/css'
              ,
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              
              "table.data { width: 300px; }"
              ,
              ".well {width: 80%; background-color: NULL; border: 0px solid rgb(255, 255, 255); box-shadow: 0px 0px 0px rgb(255, 255, 255) inset;}"
              ,
              ".tableinput .hide {display: table-header-group; color: black; align-items: center; text-align: center; align-self: center;}"
              ,
              ".tableinput-container {width: 100%; text-align: center;}"
              ,
              ".tableinput-buttons {margin: 10px;}"
              ,
              ".data {background-color: rgb(255,255,255);}"
              ,
              ".table th, .table td {text-align: center;}"
              
            ),
            h1("Introducing Smart-Shelf App"),
            p(
              "Smart Shelf is application based on BlueApp platform for demonstration and testing purposes for developers who use BlueApp Web Bluetooth library for discovering and reading data from BLE device."
              
            ),
            
            
            h3("Let's start with the Demo!!!"),
            fluidRow(
              column(width=6,
                     wellPanel(
                       h4("Input values for the training data"),
                       p("First Column- Count of items, Second Column- Total Weight"),
                       matrixInput(
                         inputId = 'data',
                         label = 'Count                           Weight',
                         data = df
                       )
                       
                     ))),
            
            fluidRow( 
              column(width = 6,
                     wellPanel(
                       h4("Training dataset: -")
                       ,
                       tableOutput(outputId = 'Gatorade_training_data')
                     ))),
            
            selectInput(
              "dataset",
              h3("Enter the item placed on scales"),
              c("Coke",
                "Gatorade",
                "Pureleaf")
            ),
            numericInput(
              "range",
              label = h3(strong("Enter the total Weight on scales")),
              0.00,
              min = 0.00,
              max = 15.00,
              step = 0.01,
              width = NULL
            ),
            
            textOutput("min_maxrange"),
            
            h3('The predicted count is:'),
            verbatimTextOutput("Extra"),
            tags$head(tags$style("#Extra {color: red;
                                 font-size: 24px;
                                 font-weight: bold;
                                 font-style: italic;
                                 }"
                         )
            )
            
            ))
          )
            )
          )))

################################ Define server logic####################################################

server <- function(input, output, session) {
  output$Gatorade_training_data <- renderTable({
    res <- input$data
    colnames(res) <- c("Count", "Weight")
    res
  }
  , include.rownames = FALSE
  , include.colnames = TRUE
  , align = "c"
  , digits = 2
  , sanitize.text.function = function(x)
    x)
  
  
  
  Gatorade_training <- reactive({
    res <- data.frame(input$data)
    colnames(res) <- c("Count", "Weight")
    res
  })
  test_data_to_display <- reactive({
    switch(
      input$dataset,
      "Coke"= Coke,
      "Gatorade" = Gatorade,
      "Pureleaf" = Pureleaf,
      "New Item"= NULL
    )
  })
  
  
  Gatorade_test<-reactive({
    test2<-test_data_to_display()
    test2
  })
  
  output$min_maxrange <- renderText({
    paste("You have chosen", input$range)
  })
  
  test_data_to_display_error <- reactive({
    test_new = Gatorade_test()
    test_new$error = NA
    test_new
  })
  
  user_data <- reactive({
    data.frame(input$range)
  })
  
  model <- reactive({
    x    <- Gatorade_training()[c(1)]
    y    <- Gatorade_training()[, c(2)]
    lm(x ~ y, data = Gatorade_training())
  })
  
  
  
  output$Extra <- renderText({
    all_test<-Gatorade_test()
    model <- lm(Weight ~ Count, data = Gatorade_training())
    sum_model <- summary(model)
    test1_prediction <-
      predict(model, newdata = all_test)
    error_intercept <- sum_model$coefficients[, 2][1] #0.07297
    error_slope <- sum_model$coefficients[, 2][2] #0.01404
    twe <-
      test_data_to_display_error()                            #where twe stands for test data set with error
    
    for (i in 1:length(twe$Count))
    {
      twe$error[i] <- error_intercept + error_slope * twe$Count[i]
      twe$Count[i]
    }
    errorless_prediction <- test1_prediction - twe$error
    errorless_prediction <- round(errorless_prediction, 2)
    twe <- cbind(twe, errorless_prediction)
    percentage_error <-
      ((twe$Weight - twe$errorless_prediction) / (twe$Weight)) * 100
    percentage_error <- round(percentage_error, digits = 2)
    twe <- cbind(twe, percentage_error)
    # Implementing logic to predict the count using given(total) weight in the user fed data
    weight_diff <- vector()
    predicted_count <- vector()
    user_new <- user_data()
    
    for (i in 1:length(user_new))
    {
      for (j in 1:length(twe$errorless_prediction))
      {
        weight_diff[j] <-
          abs(user_new$input.range[i] - twe$errorless_prediction[j])
        weight_diff[j]
      }
      min_weightDiff_index <- which.min(weight_diff[c(1:j)])
      predicted_count[i] <- twe$Count[min_weightDiff_index]
    }
    twe <- cbind(user_new, predicted_count)
    Final_output <- twe
    Final_output[, c(2)]
    
  })
  
  output$finalOutput <- renderPrint({
    paste("Predicted Count of items is", output$Extra)
  })
  
  output$model <- renderPrint({
    summary(lm(Weight ~ Count, data = Gatorade_training()))
    
  })
  
  
  output$summary <- renderPrint({
    summary(cbind(Gatorade_training()[, c(1)], Gatorade_training()[, c(2)]))
    
  })
  
  output$distPlot_dv <- renderPlot({
    x    <- Gatorade_training()[, c(2)]
    hist(
      x,
      col = 'dark gray',
      border = 'white',
      main = 'Dependent Variable',
      xlab = 'Weight'
    )
  })
  
  output$residuals_scatter <- renderPlot({
    x    <- Gatorade_training()[, c(1)]
    y    <- Gatorade_training()[, c(2)]
    model <- lm(Weight ~ Count, data = Gatorade_training())
    plot(model$residuals ~ y,
         xlab = 'Weight',
         ylab = 'Residuals')
    abline(h = 0, lty = 3)
    
  })
  
  output$distPlot_iv <- renderPlot({
    x    <- Gatorade_training()[, c(1)]
    class(x)
    hist(
      x,
      col = 'blue',
      border = 'white',
      main = 'Independent Variable',
      xlab = 'Count'
    )
    
  })
  
  # scatter plot
  
  output$scatter <- renderPlot({
    x    <- Gatorade_training()[, c(2)]
    y    <- Gatorade_training()[, c(1)]
    plot(
      y,
      x,
      xlab = 'Weight',
      ylab = 'Count',
      main = "Scatter Plot of Independent and Dependent Variables",
      pch = 16,
      col = "black",
      cex = 1
    )
    abline(lm(Weight ~ Count, data = Gatorade_training()),
           col = "red",
           lwd = 2)
  })
  isolate({
    updateTabItems(session, "tabs", "statistics")
  })
  
  
  
  output$newitem_training_data <- renderTable({
    res1 <- input$i1data
    colnames(res1) <- c("Count", "Weight")
    res1
  }
  , include.rownames = FALSE
  , include.colnames = TRUE
  , align = "c"
  , digits = 2
  , sanitize.text.function = function(x)
    x)
  output$newitem_test_data <- renderTable({
    res2 <- input$i2data
    colnames(res2) <- c("Count", "Weight")
    res2
  }
  , include.rownames = FALSE
  , include.colnames = TRUE
  , align = "c"
  , digits = 2
  , sanitize.text.function = function(x)
    x)
  
  
  new_item_training <- reactive({
    res1 <- data.frame(input$i1data)
    colnames(res1) <- c("Count", "Weight")
    res1
  })
  
  newitem_test <- reactive({
    res2 <- data.frame(input$i2data)
    colnames(res2) <- c("Count", "Weight")
    res2
  })
  
  item_weight_data <- reactive({
    data.frame(input$weight)
  })
  
  item_data_to_display_error <- reactive({
    item_new = newitem_test()
    item_new$error = NA
    item_new
  })
  
  output$item <- renderText({
    item_test<-newitem_test()
    model1 <- lm(Weight ~ Count, data = new_item_training())
    sum_model1 <- summary(model1)
    test2_prediction <-
      predict(model1, newdata = item_test)
    error_intercept <- sum_model1$coefficients[, 2][1] #0.07297
    error_slope <- sum_model1$coefficients[, 2][2] #0.01404
    twe1 <-
      item_data_to_display_error()                            #where twe stands for test data set with error
    
    for (i in 1:length(twe1$Count))
    {
      twe1$error[i] <- error_intercept + error_slope * twe1$Count[i]
      twe1$Count[i]
    }
    errorless_prediction <- test2_prediction - twe1$error
    errorless_prediction <- round(errorless_prediction, 2)
    twe1 <- cbind(twe1, errorless_prediction)
    percentage_error <-
      ((twe1$Weight - twe1$errorless_prediction) / (twe1$Weight)) * 100
    percentage_error <- round(percentage_error, digits = 2)
    twe1 <- cbind(twe1, percentage_error)
    # # Implementing logic to predict the count using given(total) weight in the user fed data
    weight_diff1 <- vector()
    predicted_count1 <- vector()
    user_item_new <-item_weight_data()
    
    
    for (i in 1:length(user_item_new))
    {
      for (j in 1:length(twe1$errorless_prediction))
      {
        weight_diff1[j] <-
          abs(user_item_new$input.weight[i] - twe1$errorless_prediction[j])
        weight_diff1[j]
      }
      min_weightDiff_index1 <- which.min(weight_diff1[c(1:j)])
      
      predicted_count1[i] <- twe1$Count[min_weightDiff_index1]
    }
    
    
    
    twe1<-cbind(user_item_new, predicted_count1)
    
    Final_output1 <- twe1
    Final_output1[, c(2)]
    
  })
  
  output$finalOutput1 <- renderPrint({
    paste("Predicted Count of items is", output$item)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}




# Run the app ----

shinyApp(ui = ui, server = server)