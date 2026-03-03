library(shiny)
library(ggplot2)
ui <- fluidPage(
inputPanel(
  sliderInput("mean", label = "Expected number of events per unit time",
              min=0, max=100, value=3.5, step=1),
  
  sliderInput("t", label = "Time Interval:",
              min = 0, max = 365, value = 1, step = 1)
),
mainPanel(
  plotOutput("poissoncurve")))

server <- function (input,output) {
  output$poissoncurve <- renderPlot({
  mu <- as.numeric(input$mean) * as.numeric(input$t)
  n <- mu + 3* sqrt(mu)
  dat <- data.frame(x=0:n,y=dpois(0:n,mu))
  ggplot(dat,aes(x,y)) +geom_col()  

})
}
shinyApp(ui=ui,server=server)
