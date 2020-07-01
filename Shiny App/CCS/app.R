#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CDDQ Clustering Solutions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Nprofiles",
                        "Number of Profiles:",
                        min = 2,
                        max = 10,
                        value = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # get the files with the profile solutions
        files <- list.files()
        ldf <- lapply(files, read.csv); remove(files)
        x    <- 
            
            
        bins <- seq(min(x), max(x), length.out = input$Nprofiles+ 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

myfile <- "https://raw.github.com/sebastiansauer/Daten_Unterricht/master/Affairs.csv"
Affairs <- read_csv(myfile)

# Run the application 
shinyApp(ui = ui, server = server)
