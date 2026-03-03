library(shiny)
ui1 <- fluidPage(
inputPanel(
  sliderInput("mn1", label = "Mean:",
              min=0, max=100, value=50, step=1),
  
  sliderInput("sd1", label = "Standard Deviation:",
              min = 0.2, max = 25, value = 10, step = 0.1)
),
 mainPanel(plotOutput("normcurve1")))
 
server1 <- function(input,output) {
 output$normcurve1 <- renderPlot({
  mn1 <- as.numeric(input$mn1)
  sd1 <- as.numeric(input$sd1)
  curve(dnorm(x,mn1,sd1),xlim=c(mn1-3*sd1,mn1+3*sd1),
        main=paste("Normal distribution with mean",mn1,
                   "and standard deviation",sd1),
        xlab="X",ylab="Density")

})
}
shinyApp(ui1, server1)

