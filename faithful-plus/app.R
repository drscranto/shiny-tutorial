#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

eruption.lm <- lm(eruptions ~ waiting, data=faithful)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Split the groups"),
        numericInput("b",
                     "Intercept of:",
                     min = -10000,
                     max = 10000,
                     step = 100,
                     value = -7000),
        numericInput("m",
                     "Slope of split:",
                     min = -100,
                     max = 100,
                     step = 10,
                     value = 100),
        h3("Time bins for time between eruptions"),
        sliderInput("waitingbins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
        br(),
        h3("Time bins for duration of eruptions"),
        numericInput("durationbins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("waitingVSduration2"),
         #plotOutput("distPlot")
        plotOutput("waitingPlot"),
        plotOutput("eruptionPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2]
   #    x <- faithful$waiting
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   #    #hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Time between eruptions")
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Time between eruptions", xlab="")
   # })
   
   output$waitingPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     x <- faithful$waiting 
     bins <- seq(min(x), max(x), length.out = input$waitingbins + 1)
     hist(x, breaks = bins, col = 'darkgray', border = 'white', main="Time between eruptions", xlab="")
   })

   output$eruptionPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     x <- faithful$eruptions
     bins <- seq(min(x), max(x), length.out = input$durationbins + 1)
     hist(x, breaks = bins, col = 'darkgray', border = 'white', main="Duration of eruptions", xlab="")
   })
   
   # output$waitingVSduration <- renderPlot({
   #   plot(faithful$waiting,faithful$eruptions,
   #        xlab="Time since last eruption", ylab="Duration of eruption",
   #        pch=19, bty="l", col="red")
   #   points(faithful$waiting,fitted.values(eruption.lm),type="l")
   # })
   
   makeGroups <- reactive({
     cutoff <- input$m*faithful$waiting + input$b
     split.bool <- faithful$waiting < cutoff
     return(list(faithful[split.bool,],faithful[!split.bool,]))
   })
   
   output$waitingVSduration2 <- renderPlot({
     split.groups <- makeGroups()
     plot(faithful$waiting,faithful$eruptions, type="n",
          xlab="Time since last eruption", ylab="Duration of eruption",
          bty="l")
     points(split.groups[[1]]$waiting, split.groups[[1]]$eruptions, col="red", pch=19)
     lm1 <- lm(eruptions~waiting,data=split.groups[[1]])
     points(split.groups[[1]]$waiting,fitted.values(lm1),type="l", col="red", lwd=2)
     lm2 <- lm(eruptions~waiting,data=split.groups[[2]])
     points(split.groups[[2]]$waiting, split.groups[[2]]$eruptions, col="blue", pch=19)
     points(split.groups[[2]]$waiting,fitted.values(lm2),type="l", col="blue", lwd=2)
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

