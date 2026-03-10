library(shiny)
ui <- fluidPage(
inputPanel(
  sliderInput("shape", label = "Shape parameter",
              min=0, max=15, value=3, step=1),
  
  sliderInput("scale", label = "Scale parameter",
              min = 0.2, max = 25, value = 10, step = 0.1)
),
mainPanel(
  plotOutput("plot")))

server <- function (input,output) {
  output$plot <- renderPlot({
  shape <- as.numeric(input$shape)
  scale <- as.numeric(input$scale)
  curve(dgamma(x,shape,scale=scale),
        xlim=c(0,100),ylim=c(0,.1),
        main=paste("Gamma distribution with shape",shape,
                   "and scale",scale),
        xlab="X",ylab="Density")

})
}
shinyApp(ui=ui,server=server)
