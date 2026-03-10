library(shiny)
Nmax <- 1000
X <- rnorm(Nmax)
Err <- rnorm(Nmax)
ui2 <- fluidPage(
inputPanel(
  selectInput("N2", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  sliderInput("rho0", label = "Correlation Coefficient:",
              min = -.25, max = .25, value = .0, step = 0.05),
  checkboxInput("sign0","Negative Correlation",FALSE)
),
mainPanel(
  plotOutput("plot2")))

server2 <- function (input,output) {
  output$plot2 <- renderPlot({
  rho <- input$rho0*ifelse(input$sign0,-1,1)
  Y <-  rho*X[1:input$N2] + sqrt(1-rho*rho)*Err[1:input$N2]
  plot(X[1:input$N2],Y,xlab="X",main=paste("Correlation =",rho))
  abline(a=0,b=rho,col="red")
},width=288,height=288)
}
shinyApp(ui=ui2,server=server2)
