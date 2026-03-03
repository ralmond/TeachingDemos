library(shiny)
ui <- fluidPage(
inputPanel(
  selectInput("tails", label = "Which tails",
              choices = c("Upper tail: Pr(t < T)"="upper",
                          "Lower tail: Pr(T < t)"="lower",
                          "Both tails: Pr(T <-t or t< T)"="both",
                          "Middle: Pr(-t < T < t)"="middle"),
              selected = "both"),
  
  numericInput("z", label = "t-value:", value=2),
  numericInput("df", label = "Degrees of Freedom (n-1 or n1 + n2 -2)", value =30)
),
mainPanel(
  plotOutput("tcurve")))

server <- function (input,output) {
  output$tcurve <- 
renderPlot({
  q <- input$z
  df <- input$df
  p <- switch(input$tails,
              upper=1-pt(q,df),
              lower=pt(q,df),
              both=2*pt(-abs(q),df),
              middle=1-2*pt(-abs(q),df))
  xl <- round(max(3,ceiling(abs(q)+.5)),1)
  curve(dt(x,df),main=paste("Probability of shaded region = ",round(p,3)),
        sub=paste("t = ",round(q,3)),
        xlim = c(-xl,xl),yaxt="n",cex=3,cex.lab=2,cex.main=2,ylab="",xlab="t")
  switch(input$tails,
         upper={
           cord.xu <- c(q,seq(q,xl,0.01),xl)
           cord.yu <- c(0,dt(seq(q,xl,0.01),df),0)
           polygon(cord.xu,cord.yu,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         },
         lower={
           cord.xl <- c(-xl,seq(-xl,q,0.01),q)
           cord.yl <- c(0,dt(seq(-xl,q,0.01),df),0)
           polygon(cord.xl,cord.yl,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         },
         both={
           q <- abs(q)
           cord.xu <- c(q,seq(q,xl,0.01),xl)
           cord.yu <- c(0,dt(seq(q,xl,0.01),df),0)
           polygon(cord.xu,cord.yu,col='plum')
           cord.xl <- c(-xl,seq(-xl,-q,0.01),-q)
           cord.yl <- c(0,dt(seq(-xl,-q,0.01),df),0)
           polygon(cord.xl,cord.yl,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
           axis(1,-q,paste(-round(q,3)),cex.axis=2)
         },
         middle={
           q <- abs(q)
           cord.xmid <- c(-q,seq(-q,q,0.01),q)
           cord.ymid <- c(0,dt(seq(-q,q,0.01),df),0)
           polygon(cord.xmid,cord.ymid,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
           axis(1,-q,paste(-round(q,3)),cex.axis=2)
         })
  
})
}
shinyApp(ui=ui,server=server)
