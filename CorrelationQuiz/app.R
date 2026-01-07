#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Correlation Quiz"),
    p("This demonstration will use some random data.  Lets start by generating the random data.  So give a ",
      a("seed"), "and pick a sample size for your sample."),
    inputPanel(
        selectInput("N", label = "Sample Size:",
              choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
        numericInput("seed", label = "Random number Seed (integer)",
              min = 0, max = .Machine$integer.max, 
              value = floor(runif(1)*.Machine$integer.max), 
              step = 157)),
    p("For each plot try to identify the strength of the correlation.  
    Note that some plots might have quadratic functions or two lines, 
    and not really be appropriate for ",em("linear")," regression."),
    p("When your done, change to the Answers tab to check your answers.  General guidelines can be found at",
      a("this page.",href="https://pluto.coe.fsu.edu/rdemos/IntroStats/CorrelationExamples.Rmd")),
    tabsetPanel(
        tabPanel("Plots",plotOutput("plot")),
        tabPanel("Answers",tableOutput("table")))
)
A2I <- c("A","B","C","D","E","F","G","H","I")
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    seed <- reactive(set.seed(input$seed))
    X <- reactive({seed(); rnorm(as.numeric(input$N))})
    Err <- reactive({seed(); rnorm(as.numeric(input$N))})
    curved <- reactive({
        seed()
        sample(c("linear","quadratic","two lines"),
        length(A2I),replace=TRUE,prob=c(7,1,1)/9)
    })
    Y <- reactive({
        Y <- matrix(NA,as.numeric(input$N),length(A2I))
        colnames(Y) <- A2I
        X <- X()
        Err <- Err()
        curved <- curved()
        for (j in 1:ncol(Y)) {
            rho <- runif(1,-1,1)
            sigma <- sqrt(1-rho*rho)
            Y[,j] <-
                switch(curved[j],
                    linear= rho*X+sigma*Err,
                    quadratic={
                        rho2 <- ifelse(runif(1)<.5,-1,1)*runif(1,.5,.85)
                        rho*X+rho2*X*X+sigma*Err},
                    "two lines"={
                        x0 <- runif(1,-.5,.5)
                        b11 <- rho
                        b12 <- rnorm(1,0,.2)
                        b02 <- (b11-b12)*x0 
                        #cat("b11",b11,"b12",b12,"b02",b02,"x0",x0,"sigma",sigma,"\n")
                        ifelse(X<x0, b11*X, b12*X + b02) + sigma*Err
                    })
            }
        Y})
    rho <- reactive(cor(X(),Y())[1,])

    output$plot <- renderPlot({
            Y <- Y()
            datasets <<- as_tibble(data.frame(X(),Y))
            datalong <<- pivot_longer(datasets,cols=colnames(Y),
                                      names_to="Y_var",values_to="Y")
            ##Don't know why converts X to X..
            ggplot(datalong,aes(X..,Y))+geom_point() +
                geom_smooth(method="lm",color="red",se=FALSE) + 
                geom_smooth(method="loess",color="blue",se=FALSE) +
                facet_wrap(vars(Y_var))
        })
    
    output$table <- renderTable(
        data.frame(correlation=round(rho(),3),curved(),
                   row.names=A2I),rownames=TRUE)
}



# Run the application 
shinyApp(ui = ui, server = server)
