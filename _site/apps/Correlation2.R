library(shiny)
ui1 <- fluidPage(
inputPanel(
  selectInput("N1", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  
  sliderInput("mx", label = "Mean of X:",
              min=0, max=100, value=50, step=1),
  
  sliderInput("sx", label = "Standard Deviation of X:",
              min = 0.2, max = 25, value = 10, step = 0.1),
  sliderInput("my", label = "Mean of Y:",
              min=0, max=100, value=50, step=1),
  
  sliderInput("sy", label = "Standard Deviation of Y:",
              min = 0.2, max = 25, value = 10, step = 0.1),
  sliderInput("rxy", label = "Correlation between X and Y:",
              min = -1, max = 1, value = 0, step = 0.05)
),
mainPanel(
  plotOutput("plot1")))

server1 <- function (input,output) {
  output$plot1 <- renderPlot({
  N <- as.numeric(input$N1)
  X <- rnorm(N)
  Err <- rnorm(N)
  rxy <- input$rxy
  mx <- input$mx
  my <- input$my
  sx <- input$sx
  sy <- input$sy
  beta1 <- sy/sx
  beta0 <- my - beta1*mx
  b1 <- rxy*beta1
  b0 <- my - b1*mx
  XX <- mx + sx*X
  YY <- sy*(rxy*X + sqrt(1-rxy*rxy)*Err) + my
  plot(XX,YY,main=paste("Regression Line (solid) y =",round(b1,2),"x + ",round(b0,2)), sub=paste("SD Line (dashed) y =",round(beta1,2),"x + ",round(beta0,2)))
  abline(a=b0,b=b1,col="red")
  abline(a=beta0,b=beta1,col="blue",lty=2)
},width=288,height=288)
}
shinyApp(ui=ui1,server=server1)
