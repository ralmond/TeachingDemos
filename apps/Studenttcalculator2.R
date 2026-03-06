library(shiny)
ui1 <- fluidPage(
  inputPanel( 
    selectInput("tails1", label = "Which tails", 
                choices = c("Upper tail: Pr(t < T)"="upper",
                            "Lower tail: Pr(T < t)"="lower", 
                            "Both tails: Pr(T <-t or t< T)"="both", 
                            "Middle: Pr(-t < T < t)"="middle"),
                selected = "both"),

  numericInput("p", label = "Probability of shaded region:", 
               value=0.05, min=0, max=1,step=.01), 
numericInput("df1", label = "Degrees of Freedom (n-1 or n1 + n2 -2)", 
             value =30) ),
mainPanel(
  plotOutput("tcurve1")))

server1 <- function (input,output) {
  output$tcurve1 <- 
renderPlot({ 
  pp <- input$p
  df1 <- input$df1 
  q <- switch(input$tails1,
              upper=qt(1-pp,df1),
              lower=qt(pp,df1),
              both=qt(1-pp/2,df1),
              middle=qt(.5+pp/2,df1))
  xl <- round(max(3,ceiling(abs(q)+.5)),1)
  curve(dt(x,df1),main=paste("Probability of shaded region = ",round(pp,3)),
        sub=paste("t(",df1,") = ",round(q,3)),
        xlim = c(-xl,xl),yaxt="n",cex=3,cex.lab=2,cex.main=2,ylab="",xlab="t")
  switch(input$tails1, 
         upper={ cord.xu <- c(q,seq(q,xl,0.01),xl) 
         cord.yu <- c(0,dt(seq(q,xl,0.01),df1),0) 
         polygon(cord.xu,cord.yu,col='plum')
         axis(1,q,paste(round(q,3)),cex.axis=2) }, 
         lower={ cord.xl <- c(-xl,seq(-xl,q,0.01),q) 
          cord.yl <- c(0,dt(seq(-xl,q,0.01),df1),0)
          polygon(cord.xl,cord.yl,col='plum')
          axis(1,q,paste(round(q,3)),cex.axis=2) }, 
         both={ q <- abs(q) 
         cord.xu <- c(q,seq(q,xl,0.01),xl) 
         cord.yu <- c(0,dt(seq(q,xl,0.01),df1),0) 
         polygon(cord.xu,cord.yu,col='plum') 
         cord.xl <- c(-xl,seq(-xl,-q,0.01),-q) 
         cord.yl <- c(0,dt(seq(-xl,-q,0.01),df1),0) 
         polygon(cord.xl,cord.yl,col='plum') 
         axis(1,q,paste(round(q,3)),cex.axis=2)
         axis(1,-q,paste(-round(q,3)),cex.axis=2) }, 
         middle={ q <- abs(q) 
         cord.xmid <- c(-q,seq(-q,q,0.01),q) 
         cord.ymid <- c(0,dt(seq(-q,q,0.01),df1),0)
         polygon(cord.xmid,cord.ymid,col='plum')
         axis(1,q,paste(round(q,3)),cex.axis=2)
         axis(1,-q,paste(-round(q,3)),cex.axis=2) 
         })

})
}
shinyApp(ui=ui1,server=server1)

