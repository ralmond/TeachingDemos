library(shiny)
Nmax <- 1000
X <- rnorm(Nmax)
Err <- rnorm(Nmax)
ui <- fluidPage(
inputPanel(
  selectInput("N", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  sliderInput("rho", label = "Correlation Coefficient:",
              min = .75, max = 1, value = .85, step = 0.05),
  checkboxInput("sign","Negative Correlation",FALSE)
),
mainPanel(
  plotOutput("plots")))

server <- function (input,output) {
  output$plots <- renderPlot({
  N <- input$N
  rho <- input$rho*ifelse(input$sign,-1,1)
  Y <-  rho*X[1:N] + sqrt(1-rho*rho)*Err[1:N]
  plot(X[1:N],Y,main=paste("Correlation =",rho),xlab="X")
  abline(a=0,b=rho,col="red")
},width=288,height=288)
}
shinyApp(ui=ui,server=server)
