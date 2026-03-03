library(shiny)
Nmax <- 1000
X <- rnorm(Nmax)
Err <- rnorm(Nmax)
ui4 <- fluidPage(
inputPanel(
selectInput("N4", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  sliderInput("b11", label = "First Slope:",
              min = -1, max = 1, value = .5, step = 0.05),
   sliderInput("b12", label = "Second Slope:",
              min = -1, max = 1, value = 0, step = 0.05),
   sliderInput("x0", label = "Crossover Point (x[0])",
              min = -1, max = 1, value = 0, step = 0.05),
   sliderInput("tau1", label = "Error Standard Deviation:",
              min = 0, max = 1, value = .5, step = 0.05)
),
mainPanel(
  plotOutput("plot4")))

server4 <- function (input,output) {
  output$plot4 <- renderPlot({
  b11 <<- input$b11
  b12 <<- input$b12
  x0 <<- input$x0
  b02 <<- (b11-b12)*x0 
  Y <<- ifelse(X<x0, b11*X, b12*X + b02) + input$tau1*Err
  rho <<- cor(X,Y)
  plot(X[1:input$N4],Y[1:input$N4],xlab="X",main=paste("Correlation =",rho))
  abline(a=input$b0,b=rho,col="red")
  abline(b=b11,a=0,col="blue",lty=2)
  abline(b=b12,a=b02,col="blue",lty=2)
},width=288,height=288)
}
shinyApp(ui=ui4,server=server4)
