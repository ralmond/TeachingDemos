library(shiny)
ui <- fluidPage(
inputPanel(
selectInput("N", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
sliderInput("rho", label = "Correlation Coefficient:",
              min = -1, max = 1, value = 0, step = 0.05)
),
mainPanel(
  plotOutput("plot")))

server <- function (input,output) {
  output$plot <- renderPlot({
  rho <- input$rho
  N <- as.numeric(input$N)
  X <- rnorm(N)
  Err <- rnorm(N)
  Y <-  rho*X + sqrt(1-rho*rho)*Err
  plot(X,Y,main=paste("Correlation =",rho))
  abline(a=0,b=rho,col="red")
},width=288,height=288)
}
shinyApp(ui=ui,server=server)
