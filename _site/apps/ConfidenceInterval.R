library(shiny)

rpointnotes <- ""
rintnotes <-""


Z <- rnorm(100)
p1 <- floor(abs(Z)) +1
ui <- fluidPage(
inputPanel(
    selectInput("M", label = "Number of Repetitions:",
                choices = c(50, 100, 200), selected = 100),
    selectInput("N", label = "Sample Size:",
                choices = c(1,5,10,25,50,100), selected = 1),
    sliderInput("my", label = "Mean of Y:",
              min=0, max=100, value=50, step=1),
    sliderInput("sy", label = "Standard Deviation of Y:",
              min = 0.2, max = 25, value = 10, step = 0.1),
    actionButton("go",label="(Re)Generate")),
mainPanel(
  plotOutput("plot"),
  fluidRow(textOutput("sem")),
  fluidRow(textOutput("text1")),
  fluidRow(textOutput("text2")),
  fluidRow(textOutput("text3")),
  tags$ul(
    tags$li("Approximaly 2/3 of the data points should be within 1 SE of the mean (plotted as circles)"),
    tags$li("* Approximately 95 percent of the data points should be within 1 SE of the mean (circles and triangles)."),
    tags$li("Approximately 5 percent of the data points should be 2 SEs or more away from the mean (plotted at diamonds).")),
  p("Note that changing the mean and sd of the population only changes the scales on the graph, not the structure of the problem."),
  h2("Random Intervals"),
  p("Taking the sample mean and going plus or minus two standard errors produces a confidence interval."),
  markdown("Actually, the two standard error rule is based on looking up the .975 (1-.05/2) point on the [normal table](NormalCalculator.Rmd).  We could put other values in there as well (50%, 75%, 90% and 99% are common choices).  This will adjust the length of the slider."),
  sliderInput("alpha","Confidence",
                       min=0,max=1,value=.95,step=.01),
  plotOutput("plotI"))
)

server <- function (input,output) {

  dataSet <- reactiveValues(Z=Z,sem=10,
  	                         pch=ifelse(p1 >2, 5, p1), 
             			 M=100,
				 my=50,sy=10)
  
  observeEvent(input$go,{
      M <- as.numeric(input$M)
      dataSet$Z <- rnorm(M)
      sy <- as.numeric(input$sy)
      dataSet$sem <- sy / sqrt(as.numeric(input$N))
      p1 <-floor(abs(dataSet$Z)) + 1
      dataSet$pch <- ifelse(p1 > 2, 5, p1)
      my <- as.numeric(input$my)
  })

  output$plot <- renderPlot({
      sem <- dataSet$sem
      my <- dataSet$my
      sy <- dataSet$sy
      X <- dataSet$Z * sem + my
      M <- length(X)
      curve(
        dnorm(x, my, sem),
        xlim = c(my - 3.5 * sy, 
                 my + 3.5 * sy),
        ylab = "density",
        xlab = "Sample Mean"
      )
      abline(v = my)
      abline(h = 0)
      text(my + .25 * sem, .02, expression(mu[Y]))
      abline(v = my - 2 * sem)
      text(my - 2 * sem + .25 * sem, 0.0025, "-2SE")
      abline(v = my - sem)
      text(my - sem + .25 * sem, 0.005,"-1SE")
      abline(v = my + sem)
      text(my + sem + .25 * sem, 0.005, "+1SE")
      abline(v = my + 2 * sem)
      text(my + 2 * sem + .25 * sy, 0.0025, "2SE")
      points(X, rep(0, M), pch = dataSet$pch)
    })
   output$sem <- renderText({
     sem <- dataSet$sem
      paste(
        "Standard Error = ",
        round(sem, 3),".")})
   output$text1 <- renderText({
     pch <- dataSet$pch
     paste(sum(pch == 1),
        "Estimates less than 1 SE from mean;")
    })
   output$text2 <- renderText({
     pch <- dataSet$pch
     paste(sum(pch == 2),
        "Estimates between 1 and 2 SE from mean;")
    })
   output$text3 <- renderText({
     pch <- dataSet$pch
     paste(sum(pch == 5),
        "Estimates more than 2 SE from mean.")
   })
   output$plotI <- renderPlot({
      sem <- dataSet$sem
      my <- dataSet$my
      X <- dataSet$Z * sem + my
      M <- length(X)
      i <- 1:M
      alpha <- (1-as.numeric(input$alpha))/2
      X.low <- X +qnorm(alpha)*sem
      X.high <- X + qnorm(1-alpha)*sem
      pch1 <- ifelse(X.low <= my & my <= X.high,1,5)
      plot(c(my-3.5*sem,my+3.5*sem),c(0,M+1),
             ylab="Trial",xlab="Sample Mean",
           main=paste(100*(1-2*alpha),"% Confidence Intervals"), type="n")
      points(X,i,pch=pch1)
      segments(X.low,i,X.high,i,lty=pch1,
               col=ifelse(pch1==5,1,5))
      abline(v = my)
    })



}
shinyApp(ui=ui,server=server)
