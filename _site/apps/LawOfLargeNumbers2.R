library(shiny)

nmax <- 1000
rdist <- list(Normal=rnorm, Exponential = rexp, 
              Gamma = function(n) rgamma(n,3),
              "T" = function(n) rt(n,3))
pdist <- list(Normal=pnorm, Exponential = pexp, 
              Gamma = function(q) pgamma(q,3),
              "T" = function(q) pt(q,3))
ddist <- list(Normal=dnorm, Exponential = dexp, 
              Gamma = function(x) dgamma(x,3),
              "T" = function(x) dt(x,3))

ui1 <- fluidPage(
inputPanel(
  selectInput("dist",label="Distribution Type",
              choices=c("Normal","Exponential","Gamma","T"),
              selected="Normal"),
   sliderInput("NN", label = "Maximum Sample Size:",
              min = 25, max=nmax, value=100, step=5)

),
mainPanel(
  plotOutput("dists")))

server1 <- function (input,output) {
  output$dists <- renderPlot({
  XX <- do.call(rdist[[input$dist]],list(nmax))
  Fn <-ecdf(XX[1:input$NN])
  layout(matrix(c(1,2),1,2))
  
  plot(Fn, main=paste("Actual vs Empirical Distribution Function, N=",input$NN))
  curve(do.call(pdist[[input$dist]],list(x)),add=TRUE,lty=2,col="red")
  
  hist(XX[1:input$NN], probability = TRUE,
       main=paste("Actual vs Empirical Density Function, N=",input$NN),xlab="X")
  curve(do.call(ddist[[input$dist]],list(x)),add=TRUE,lty=2,col="red")
  
})
}
shinyApp(ui=ui1,server=server1)
