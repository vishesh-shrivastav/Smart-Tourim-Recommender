library(shiny)
library(shinythemes)
setwd("C:\\Users\\VShrivastav\\Desktop\\AI\\Smart-Tourim-Recommender")

# Read in the data from Github
data.cities <- read.csv("https://raw.github.umn.edu/munja004/CS5511-Project/devel/data_cities_1201.csv?token=AAA1y2LwGG6X25u2cSpD0NDNZPBoNW0pks5cGWOJwA%3D%3D", header = T)
data.nature <- read.csv("https://raw.github.umn.edu/munja004/CS5511-Project/master/data_nature_1201.csv?token=AAA1yw_3sPirClbJMwGm1hkEqd4Qshquks5cGWUewA%3D%3D", header = T)

# Read in the data from stored csv
# data.cities <- read.csv("data_cities.csv")
# data.nature <- read.csv("data_nature.csv")

# Drop irrelevant columns - "Fun...Games" and "Nature...Parks"
#data.cities <- within(data.cities, rm("Fun...Games", "Nature...Parks"))
data.cities <- within(data.cities, rm("Nature...Parks"))

colnames(data.cities) <- c("label", "Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                           "Concerts.Shows", "Museums", "Fun.Games.Activities", "Food.Drink")
colnames(data.nature) <- c("label", "Outdoor.Activities", "Natural.Beauty", "Sights.Landmarks", "Tours", "Adrenaline")

ui <- fluidPage(theme = shinytheme("darkly"),
  
  # Application title
  titlePanel("Smart Tourism Recommender"),
  sidebarPanel(
    textOutput("text2"), br(),
    radioButtons("radio1", 
                 label = "Select Destination Type:",
                 choices = list("Nature", "Metropolitan"), 
                 selected = 'Nature'),
    textOutput("text1"), 
    br(),
    conditionalPanel(
      condition = "input.radio1 == 'Metropolitan'",
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
    conditionalPanel(
      condition = "input.radio1 == 'Nature'",
      sliderInput("out.act", "Activities", min = 1, max = 10, value = 5, step = 0.01),
      sliderInput("nat.beau", "Natural Beauty", min = 1, max = 10, value = 5, step = 0.01),
      sliderInput("sl", "Sights/Landmarks", min = 1, max = 10, value = 5, step = 0.01),
      sliderInput("gtours", "Guided Tours", min = 1, max = 10, value = 5, step = 0.01),
      sliderInput("adre", "Adrenaline", min = 1, max = 10, value = 5, step = 0.01)
    )
    
  ),
  mainPanel(
    # fluidRow(column(12, align = 'center', actionButton("go", "Go"))),
    # hr(),
    # fluidRow(column(12, align = 'center', textOutput("result.text"))),
    # hr(),
    # fluidRow(column(12, align = 'center', textOutput("result.budget")))
    fluidRow(column(12, align = 'left', actionButton("go", "Go"))),
    hr(),
    fluidRow(column(12, align = 'left', textOutput("result.text"))),
    hr(),
    fluidRow(column(12, align = 'left', textOutput("result.budget")))
  )
)

# Define server logic
server <- function(input, output) {
  options(warn = -1)
  output$text1 <- renderText(paste0("On a scale of 1-10, with 1 being the least and 10 being the most,
                                    how much would you like to do each of the following activity?"))
  
  output$text2 <- renderText(paste0("Please select whether you want to visit a metropolitan or explore the nature?"))
  
  # Create a vector by combining all the inputs when the user clicks on 'Go'
  new_vector <- eventReactive(input$go, {
    if (input$radio1 == 'Metropolitan'){
      vec <- cbind(input$shop, input$night, input$gtours, input$spa.well, input$sl, input$cshows, input$muse, input$out, input$fand)
    }
    else if (input$radio1 == 'Nature'){
      vec <- cbind(input$out.act, input$nat.beau, input$sl, input$gtours, input$sl, input$adre)
    }
    return(vec)
  })
  
  # Function to calculate Euclidean distance of input vector with a row
  calculate.euclidean <- function(x){
    return(dist(rbind(x, new_vector())))
  }
  
  # Subset data, keep only those columns which will be used in calculating Euclidean distance
  new_df <- reactive({
    if (input$radio1 == 'Metropolitan'){
      foo <- subset(data.cities, select = c("Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                                            "Concerts.Shows", "Museums", "Fun.Games.Activities", "Food.Drink"))
    }
    
    else if (input$radio1 == 'Nature'){
      foo <- subset(data.nature, select = c("Outdoor.Activities", "Natural.Beauty", "Sights.Landmarks", "Tours", "Adrenaline"))
    }
    
    return(foo)
  })
  
  result.destination <- reactive({
    # Apply calculate.euclidean() to each row of the subsetted data
    cf <- apply(new_df(), 1, calculate.euclidean)
    # Add computed distance as a column to the existing data frame
    if (input$radio1 == 'Metropolitan'){
      data.cities$distance <- cf
      data.cities.ordered <- data.cities[order(data.cities$distance),]
      baz <- data.cities.ordered[1, "label"]
    }
    else if (input$radio1 == 'Nature'){
      data.nature$distance <- cf
      data.nature.ordered <- data.nature[order(data.nature$distance),]
      baz <- data.nature.ordered[1, "label"]
    }
    return(baz)
  })
  
  output$result.text <- renderText({
    paste0("You should go to: ", result.destination())
  })
  
  output$result.budget <- renderText({
    paste0("Approximate budget for your trip: $100 per day")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)