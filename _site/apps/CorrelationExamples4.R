library(shiny)
Nmax <- 1000
X <- rnorm(Nmax)
Err <- rnorm(Nmax)
ui3 <- fluidPage(
inputPanel(
  selectInput("N3", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  sliderInput("b2", label = "Quadradic Term Slope:",
              min = -1, max = 1, value = .5, step = 0.05),
   sliderInput("b1", label = "Linear Term Slope:",
              min = -1, max = 1, value = 0, step = 0.05),
   sliderInput("b0", label = "Intercept:",
              min = -1, max = 1, value = 0, step = 0.05),
   sliderInput("tau", label = "Error Standard Deviation:",
              min = 0, max = 1, value = .5, step = 0.05)
),
mainPanel(
  plotOutput("plot3")))

server3 <- function (input,output) {
  output$plot3 <- renderPlot({
  Y <-  input$b2*X*X + input$b1*X + input$b0 + input$tau*Err
  rho <- cor(X,Y)
  plot(X[1:input$N3],Y[1:input$N3],xlab="X",main=paste("Correlation =",rho))
  abline(a=input$b0,b=rho,col="red")
  lines(lowess(X,Y),col="blue",lty=2)
},width=288,height=288)
}
shinyApp(ui=ui3,server=server3)
