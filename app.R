library(shiny)
setwd("C:\\Users\\VShrivastav\\Desktop\\AI\\Smart-Tourim-Recommender")
# Read in the data
data.cities <- read.csv("data_cities.csv")
data.nature <- read.csv("data_nature.csv")

# Drop irrelevant columns - "Fun...Games" and "Nature...Parks"
data.cities <- within(data.cities, rm("Fun...Games", "Nature...Parks"))

colnames(data.cities) <- c("label", "Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                           "Concerts.Shows", "Museums", "Outdoor.Activities", "Food.Drink")

#print(colnames(data.cities))
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Smart Tourism Recommender"),
      # Show a plot of the generated distribution
      sidebarPanel(
        sliderInput("shop", "Shopping", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("night", "Nightlife", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("gtours", "Guided Tours", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("spa.well", "Spas/Wellness", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("sl", "Sights/Landmarks", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("cshows", "Concerts/Shows", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("muse", "Museums", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("out", "Outdoor Activities", min = 1, max = 10, value = 5, step = 0.01),
        sliderInput("fand", "Food and Drink", min = 1, max = 10, value = 5, step = 0.01)
      ),
   mainPanel(
     actionButton("go", "Go"),
     textOutput("text1"),
     textOutput("text2"),
     tableOutput("new.table"),
     textOutput("result.text")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  new_vector <- eventReactive(input$go, {
    xx <- cbind(input$shop, input$night, input$gtours, input$spa.well, input$sl, input$cshows, input$muse, input$out, input$fand)
    #print(xx)
    #print(input$shop)
    return(xx)
  })
  
  output$text1 <- renderText(mean(new_vector()))
  
  # Function to calculate Euclidean distance of input vector with a row
  calculate.euclidean <- function(x){
    return(dist(rbind(x, new_vector())))
  }
  
  new_df <- reactive({
    subset(data.cities, select = c("Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                                   "Concerts.Shows", "Museums", "Outdoor.Activities", "Food.Drink"))
  })
  
  xz <- reactive({
    cf <- apply(new_df(), 1, calculate.euclidean)
    print(cf)
    data.cities$distance <- cf
    df.cities.ordered <- data.cities[order(data.cities$distance),]
    return(cf)
  })
  
  output$text2 <- renderText(mean(xz()))
  
  new.table <- reactive({
    cf <- apply(new_df(), 1, calculate.euclidean)
    data.cities$distance <- cf
    df.cities.ordered <- data.cities[order(data.cities$distance),]
  })
  
  output$new.table <- renderTable(new.table())
  
  result.destination <- reactive({
    cf <- apply(new_df(), 1, calculate.euclidean)
    data.cities$distance <- cf
    df.cities.ordered <- data.cities[order(data.cities$distance),]
    df.cities.ordered[1, "label"]
  })
  
  output$result.text <- renderText({
    paste0("You should go to: ", result.destination())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)