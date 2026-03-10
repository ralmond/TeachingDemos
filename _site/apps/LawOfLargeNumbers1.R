library(shiny)
ui <- fluidPage(
inputPanel(
  selectInput("N", label = "Maximum Sample Size:",
              choices = c(50, 100, 200, 500, 1000), selected = 200),
  
  sliderInput("p", label = "Probability of event (p)",
              min = 0, max = 1, value = .5, step = 0.01),
  sliderInput("delta", label = "Distance of reference line from target (delta)",
              min = 0, max = .1, value = .05, step = 0.005)

),
mainPanel(
  plotOutput("converge")))

server <- function (input,output) {
  output$converge <- renderPlot({
  x <- runif(input$N) < input$p
  pn <- cumsum(x)/1:input$N
  plot(1:input$N,pn,xlab="Number of Trials",ylab="Proportion Success",
       type="l")
  abline(h=input$p,col="blue")
  abline(h=input$p+input$delta,col="skyblue")
  abline(h=input$p-input$delta,col="skyblue")
})
}
shinyApp(ui=ui,server=server)
