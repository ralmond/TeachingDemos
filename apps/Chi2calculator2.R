library(shiny)
ui1 <- fluidPage(
inputPanel(
   selectInput("tails1", label = "Which tails",
              choices = c("Upper tail: Pr(x^2 < X^2)"="upper",
                          "Lower tail: Pr(X^2 < x^2)"="lower"),
              selected = "upper"),
  numericInput("pp", label = "Probability of shaded region:", value=0.05, min=0, max=1, step=.01),
  numericInput("df1", label = "Degrees of Freedom", value =1)
),
mainPanel(
  plotOutput("quantplot")))

server1 <- function (input,output) {
  output$quantplot <- renderPlot({
  pp <- as.numeric(input$pp)
  df1 <- as.numeric(input$df1)
  tails1 <- input$tails1
  if (is.null(tails1)) tails1 <- "upper"
  qq <- switch(tails1,
              upper=qchisq(1-pp,df1),
              lower=qchisq(pp,df1))
 xl1 <- round(qchisq(.999,df1),1)
  curve(dchisq(x,df1),main=paste("Probability of shaded region = ",round(pp,3)),
        sub=paste("X2(",df1,") = ",round(qq,3)),
        xlim = c(0,xl1),yaxt="n",cex=3,cex.lab=2,cex.main=2,ylab="",xlab="Chi-squared")
   switch(tails1,
         upper={
           cord.xu1 <- c(qq,seq(qq,xl1,0.01),xl1)
           cord.yu1 <- c(0,dchisq(seq(qq,xl1,0.01),df1),0)
           polygon(cord.xu1,cord.yu1,col='plum')
           axis(1,qq,paste(round(qq,3)),cex.axis=2)
         },
         lower={
           cord.xl1 <- c(0,seq(0,qq,0.01),q)
           cord.yl1 <- c(0,dchisq(seq(0,qq,0.01),df1),0)
           if(!is.finite(cord.yl[2])) cord.yl[2] <- cord.yl[3]
           polygon(cord.xl1,cord.yl1,col='plum')
           axis(1,qq,paste(round(qq,3)),cex.axis=2)
         })

})
}
shinyApp(ui=ui1,server=server1)
