Nmax <- 1000
X <- rnorm(Nmax)
Err <- rnorm(Nmax)
library(shiny)
ui1 <- fluidPage(
inputPanel(
  selectInput("N1", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  sliderInput("rho1", label = "Correlation Coefficient:",
              min = .25, max = .75, value = .5, step = 0.05),
  checkboxInput("sign1","Negative Correlation",FALSE)
),
mainPanel(
  plotOutput("plot1")))

server1 <- function (input,output) {
  output$plot1 <- renderPlot({
  rho <<- input$rho1*ifelse(input$sign1,-1,1)
  Y <<-  rho*X[1:input$N1] + sqrt(1-rho*rho)*Err[1:input$N1]
  plot(X[1:input$N1],Y,xlab="X",main=paste("Correlation =",rho))
  abline(a=0,b=rho,col="red")
},width=288,height=288)
}
shinyApp(ui=ui1,server=server1)
