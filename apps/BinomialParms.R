
library(shiny)
library(ggplot2)
ui <- fluidPage(
inputPanel(
  sliderInput("n", label = "Number of draws:",
              min=0, max=100, value=10, step=1),
  
  sliderInput("p", label = "Probability of success:",
              min = 0, max = 1, value = .6, step = 0.01)
),
mainPanel(
  plotOutput("bincurve")))

server <- function (input,output) {
  output$bincurve <- renderPlot({
  n <- as.numeric(input$n)
  p <- as.numeric(input$p)
  dat <- data.frame(x=0:n,y=dbinom(0:n,n,p))
  ggplot(dat,aes(x,y)) +geom_col()  

})
}
shinyApp(ui=ui,server=server)
