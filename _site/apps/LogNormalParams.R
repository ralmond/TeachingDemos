library(shiny)
ui <- fluidPage(
inputPanel(
  sliderInput("mn", label = "Mean Log:",
              min=-50, max=50, value=0, step=1),
  
  sliderInput("sd", label = "Standard Deviation Log:",
              min = 0.2, max = 10, value = 2, step = 0.1)
),
mainPanel(
  plotOutput("lognormcurve")))

server <- function (input,output) {
  output$lognormcurve <- renderPlot({
  mn <- as.numeric(input$mn)
  sd <- as.numeric(input$sd)
  xlim <- qlnorm(c(.001,.999),mn,sd)
  curve(dlnorm(x,mn,sd),xlim=xlim,
        main=paste("Lognormal distribution with mean log",mn,
                   "and log standard deviation",sd),
        xlab="X",ylab="Density",log="x")

})
}
shinyApp(ui=ui,server=server)

