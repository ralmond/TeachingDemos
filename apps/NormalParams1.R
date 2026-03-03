library(shiny)
ui <- fluidPage(
  inputPanel(
  sliderInput("mn", label = "Mean:",
              min=0, max=100, value=50, step=1),
  
  sliderInput("sd", label = "Standard Deviation:",
              min = 0.2, max = 25, value = 10, step = 0.1)
),
mainPanel(
  plotOutput("normcurve")))

server <- function (input,output) {
  output$normcurve <- 
  renderPlot({
  mn <- as.numeric(input$mn)
  sd <- as.numeric(input$sd)
  curve(dnorm(x,mn,sd),xlim=c(0,100),ylim=c(0,.1),
        main=paste("Normal distribution with mean",mn,
                   "and standard deviation",sd),
        xlab="X",ylab="Density")

})
}
shinyApp(ui=ui,server=server)
