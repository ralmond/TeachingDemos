library(shiny)
library(tidyverse)
ui <- fluidPage(
inputPanel(
  selectInput("N", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
  
  numericInput("seed", label = "Random number Seed (integer)",
              min = 0, max = .Machine$integer.max, 
              value = floor(runif(1)*.Machine$integer.max), 
              step = 157)
),
mainPanel(
  plotOutput("scatterplots"),
  tableOutput("answers"))
)

server <- function (input,output) {

  data <- reactive({
	  N <- as.numeric(input$N)
  	set.seed(input$seed)
  	X<-rnorm(N)
  	Err<-rnorm(N)
  	rho <- runif(9,-1,1)
    names(rho) <- c("A","B","C","D","E","F","G","H","I")
    Y <- matrix(NA,N,9)
    colnames(Y) <- names(rho)
    ## Linear 
    for (j in 1:9)
       Y[,j] <- rho[j]*X+sqrt(1-rho[j]*rho[j])*Err
    ## Select some for curves
    curves <- sample.int(9,rbinom(1,9,.3))
    for (j in curves) {
    
      if (runif(1) < .5) {
        ## Quadratic
        rho2 <- ifelse(runif(1)<.5,-1,1)*runif(1,.5,.85)
        sigma <- sqrt(1-rho[j]*rho[j])
        Y[,j] <- rho[j]*X+rho2*X*X+sigma*Err
        rho[j] <- cor(X,Y[,j])
      } else {
        ## Split curve
        sigma <- sqrt(1-rho[j]*rho[j])
        x0 <- runif(1,-.5,.5)
        b11 <- rho[j]
        b12 <- rnorm(1,0,.2)
        b02 <- (b11-b12)*x0 
        Y[,j] <- ifelse(X<x0, b11*X, b12*X + b02) + sigma*Err
        rho[j] <- cor(X,Y[,j])
     }
  }
  list(X=X,Y=Y,rho=rho,curves=curves)
  })


  output$scatterplots <- renderPlot({
  N <- as.numeric(input$N)
  X <- data()$X
  Y <- data()$Y
  rho <- data()$rho
 
  datasets <- as_tibble(data.frame(X,Y))
  datalong <- pivot_longer(datasets,cols=colnames(Y),
                            names_to="Y_var",values_to="Y")
  
 ggplot(datalong,aes(X,Y))+geom_point() +
    geom_smooth(method="lm",color="red",se=FALSE) + 
    geom_smooth(method="loess",color="blue",se=FALSE) +
    facet_wrap(vars(Y_var))
}, height=800,width=800)

output$answers <- renderTable(data.frame(correlation=round(data()$rho,3),
                                         curved=(1:9%in%data()$curves)),
                              row.names=names(data()$rho),
                              rownames=TRUE)
}
shinyApp(ui=ui,server=server)

