library(shiny)
ui <- fluidPage(
inputPanel(
  selectInput("tails", label = "Which tails",
              choices = c("Upper tail: Pr(z < Z)"="upper",
                          "Lower tail: Pr(Z < z)"="lower",
                          "Both tails: Pr(Z <-z or z< Z)"="both",
                          "Middle: Pr(-z < Z < z)"="middle"),
              selected = "both"),
  
  numericInput("z", label = "Normal quantile (z):", value=2)
),
mainPanel(
  plotOutput("normcurve")))

server <- function (input,output) {
  output$normcurve <- 
renderPlot({
  q <- input$z
  p <- switch(input$tails,
              upper=1-pnorm(q),
              lower=pnorm(q),
              both=2*pnorm(-abs(q)),
              middle=1-2*pnorm(-abs(q)))
  xl <- round(max(3,ceiling(abs(q)+.5)),1)
  curve(dnorm(x),main=paste("Probability of shaded region = ",round(p,3)),
        sub=paste("z = ",round(q,3)),
        xlim = c(-xl,xl),yaxt="n",cex=3,cex.lab=2,cex.main=2,ylab="",xlab="z")
  switch(input$tails,
         upper={
           cord.xu <- c(q,seq(q,xl,0.01),xl)
           cord.yu <- c(0,dnorm(seq(q,xl,0.01)),0)
           polygon(cord.xu,cord.yu,col='skyblue')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         },
         lower={
           cord.xl <- c(-xl,seq(-xl,q,0.01),q)
           cord.yl <- c(0,dnorm(seq(-xl,q,0.01)),0)
           polygon(cord.xl,cord.yl,col='skyblue')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         },
         both={
           q <- abs(q)
           cord.xu <- c(q,seq(q,xl,0.01),xl)
           cord.yu <- c(0,dnorm(seq(q,xl,0.01)),0)
           polygon(cord.xu,cord.yu,col='skyblue')
           cord.xl <- c(-xl,seq(-xl,-q,0.01),-q)
           cord.yl <- c(0,dnorm(seq(-xl,-q,0.01)),0)
           polygon(cord.xl,cord.yl,col='skyblue')
           axis(1,q,paste(round(q,3)),cex.axis=2)
           axis(1,-q,paste(-round(q,3)),cex.axis=2)
         },
         middle={
           q <- abs(q)
           cord.xmid <- c(-q,seq(-q,q,0.01),q)
           cord.ymid <- c(0,dnorm(seq(-q,q,0.01)),0)
           polygon(cord.xmid,cord.ymid,col='skyblue')
           axis(1,q,paste(round(q,3)),cex.axis=2)
           axis(1,-q,paste(-round(q,3)),cex.axis=2)
         })
  
})
}
shinyApp(ui=ui,server=server)
