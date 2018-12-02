library(shiny)
#setwd("C:\\Users\\VShrivastav\\Desktop\\AI\\Smart-Tourim-Recommender")
setwd("C:\\Users\\Shrivatav\\Desktop\\UMN\\Homework\\Fall 2018\\AI 1\\Project")
# Read in the data
data.cities <- read.csv("data_cities_1201.csv")
data.nature <- read.csv("data_nature_1201.csv")

# Drop irrelevant columns - "Fun...Games" and "Nature...Parks"
data.cities <- within(data.cities, rm("Nature...Parks"))

colnames(data.cities) <- c("label", "Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                           "Concerts.Shows", "Museums", "Fun.Games.Activities", "Food.Drink")

colnames(data.nature) <- c("label", "Outdoor.Activities", "Natural Beauty", "Sights.Landmarks", "Tours", "Adrenaline")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Smart Tourism Recommender"),
  sidebarPanel(
    textOutput("text1"), br(),
    sliderInput("shop", "Shopping", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("night", "Nightlife", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("gtours", "Guided Tours", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("spa.well", "Spas/Wellness", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("sl", "Sights/Landmarks", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("cshows", "Concerts/Shows", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("muse", "Museums", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("out", "Fun/Games/Activities", min = 1, max = 10, value = 5, step = 0.01),
    sliderInput("fand", "Food and Drink", min = 1, max = 10, value = 5, step = 0.01)
  ),
  mainPanel(
    fluidRow(column(12, align = 'center', actionButton("go", "Go"))),
    hr(),
    fluidRow(column(12, align = 'center', textOutput("result.text")))
  )
)

# Define server logic
server <- function(input, output) {
  output$text1 <- renderText(paste0("On a scale of 1-10, with 1 being the least and 10 being the most,
                                    how much would you like to do each of the following activity?"))
  
  # Create a vector by combining all the inputs when the user clicks on 'Go'
  new_vector <- eventReactive(input$go, {
    vec <- cbind(input$shop, input$night, input$gtours, input$spa.well, input$sl, input$cshows, input$muse, input$out, input$fand)
    return(vec)
  })
  
  # Function to calculate Euclidean distance of input vector with a row
  calculate.euclidean <- function(x){
    return(dist(rbind(x, new_vector())))
  }
  
  # Subset data, keep only those columns which will be used in calculating Euclidean distance
  new_df <- reactive({
    subset(data.cities, select = c("Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                                   "Concerts.Shows", "Museums", "Fun.Games.Activities", "Food.Drink"))
  })
  
  result.destination <- reactive({
    # Apply calculate.euclidean() to each row of the subsetted data
    cf <- apply(new_df(), 1, calculate.euclidean)
    # Add computed distance as a column to the existing data frame
    data.cities$distance <- cf
    # Order new data by distance in ascending order
    df.cities.ordered <- data.cities[order(data.cities$distance),]
    # Return 'label' column from the first row
    # This is the destination which is most similar to the input vector
    df.cities.ordered[1, "label"]
  })
  
  output$result.text <- renderText({
    paste0("You should go to: ", result.destination())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)