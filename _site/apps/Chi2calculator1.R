library(shiny)
ui <- fluidPage(
inputPanel(
  selectInput("tails", label = "Which tails",
              choices = c("Upper tail: Pr(x^2 < X^2)"="upper",
                          "Lower tail: Pr(X^2 < x^2)"="lower"),
              selected = "upper"),
  
  numericInput("x2", label = "chi-squared value:", value=2),
  numericInput("df", label = "Degrees of Freedom", value =1)
),
mainPanel(
  plotOutput("probplot")))

server <- function (input,output) {
  output$probplot <- renderPlot({
  q <- input$x2
  df <- input$df
  p <- switch(input$tails,
              upper=1-pchisq(q,df),
              lower=pchisq(q,df))
  xl <- round(qchisq(.999,df),1)
  curve(dchisq(x,df),main=paste("Probability of shaded region = ",round(p,3)),
        sub=paste("chi-squared = ",round(q,3)),
        xlim = c(0,xl),yaxt="n",cex=3,cex.lab=2,cex.main=2,ylab="",xlab="Chi-squared")
  switch(input$tails,
         upper={
           cord.xu <- c(q,seq(q,xl,0.01),xl)
           cord.yu <- c(0,dchisq(seq(q,xl,0.01),df),0)
           polygon(cord.xu,cord.yu,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         },
         lower={
           cord.xl <- c(0,seq(0,q,0.01),q)
           cord.yl <- c(0,dchisq(seq(0,q,0.01),df),0)
           if(!is.finite(cord.yl[2])) cord.yl[2] <- cord.yl[3]
           polygon(cord.xl,cord.yl,col='plum')
           axis(1,q,paste(round(q,3)),cex.axis=2)
         })
})
}
shinyApp(ui=ui,server=server)
