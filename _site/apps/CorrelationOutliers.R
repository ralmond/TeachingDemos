library(shiny)
ui <- fluidPage(
inputPanel(
  selectInput("N", label = "Sample Size:",
              choices = c(10, 25, 50, 100, 250, 500), selected = 25),
  sliderInput("x1", label = "X-coordinate of point 1:",
              min = -5, max = 5, value = 0, step = 0.05),
  sliderInput("y1", label = "Y-coordinate of point 1:",
              min = -5, max = 5, value = 0, step = 0.05)
),
mainPanel(
  plotOutput("scatterplot")))

server <- function (input,output) {
  output$scatterplot <- renderPlot({
  N <- as.numeric(input$N)
  X <- c(0,rnorm(N-1))
  Y <- c(0,rnorm(N-1))
  pch <- c(19,rep(1,N-1))
  pcol <- c("red",rep("gray",N-1))
  X[1] <- input$x1
  Y[1] <- input$y1
  plot(X,Y,xlim=c(-5,5),ylim=c(-5,5),pch=pch,col=pcol,
       main=paste("Correlation = ",round(cor(X,Y),3)),
       sub=paste("Correlation without point 1 = ",round(cor(X[-1],Y[-1]),3)))
  abline(a=0,b=cor(X,Y),col="blue")
},width=288,height=288)
}
shinyApp(ui=ui,server=server)
