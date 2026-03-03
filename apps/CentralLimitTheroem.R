library(shiny)
nmax <- 1000
mmax <- 100
rdist <- list(Uniform=runif,
              Binomial= function(n) rbinom(n,1,.5),
              Exponential = rexp, 
              Gamma = function(n) rgamma(n,3),
              "T" = function(n) rt(n,3))

ui <- fluidPage(
inputPanel(
  selectInput("dist",label="Distribution Type",
              choices=c("Uniform","Binomial","Exponential","Gamma","T"),
              selected="Unifor"),
   sliderInput("NN", label = "Number of Samples:",
              min = 25, max=nmax, value=nmax, step=5),
  sliderInput("MM",label="Size of each sample:", min=1, max=mmax,value=1,step=1)

),
mainPanel(
  plotOutput("convplot")))

server <- function (input,output) {

  XX <- reactive(matrix(do.call(rdist[[input$dist]],list(nmax*mmax)),nmax,mmax))

  output$convplot <- renderPlot({
  layout(matrix(1:4,2,2))
  X1 <- XX()[1:input$NN,1]
  Xmean <- rowMeans(XX()[1:input$NN,1:input$MM,drop=FALSE])

  hist(X1,main="Average of sample of size 1",probability=TRUE)
  curve(dnorm(x,mean(X1),sd(X1)),add=TRUE,lty=2,col="red")

  qqnorm(X1,main="Average of sample of size 1")
  qqline(X1)

  hist(Xmean,
       main=paste("Average of sample of Size",input$MM),probability=TRUE)
  curve(dnorm(x,mean(Xmean),sd(Xmean)),add=TRUE,lty=2,col="red")

  qqnorm(Xmean,main=paste("Average of sample of Size",input$MM))
  qqline(Xmean)
  
  
  
})
}
shinyApp(ui=ui,server=server)
