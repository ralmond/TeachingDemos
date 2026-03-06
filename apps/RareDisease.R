library(shiny)
ui <- fluidPage(
inputPanel(
  numericInput("baserate", label = "Base Rate (Pr(D=Y)):", value=.0000458,width=100),
 numericInput("sensitivity", label = "Sensitivity (Pr(T=+|D=Y)):", value=.972,width=100),
numericInput("specificity", label = "Specificity (Pr(T=-|D=N)):", value=.92,width=100)
),
mainPanel(
  tableOutput("tab"),
  h2("Calculating true positive and false positive"),
  markdown("One way to calculate this is to use *Bayes' theorem*. However, from the table above, it is easy to calculate the true positive and false positive rates. We now just look at the columns of the table."),
  p(strong("False Positive Rate"),"Pr(D=N|T=+)",textOutput("tpr")),
  p(strong("False Negative Rate"),"Pr(D=Y|T=-)",textOutput("fpr")),
  h3("What is going on???"),
  p("That false positive rate seems very high. What is really going on? The root cause is that as of this writing (Mar 22, 2020) COVID-19 is still pretty rare. So although getting a false positive is rare, actually having COVID-19 is much rarer. The following picture might help:"),
  shiny::plotOutput("mosaic")
))

server <- function (input,output) {

  tab <- reactive(
    matrix(c(input$baserate*input$sensitivity,
           input$baserate*(1-input$sensitivity),
           (1-input$baserate)*(1-input$specificity),
           (1-input$baserate)*input$specificity),2,2,
         byrow=TRUE,
         dimnames=list(D=c("Y","N"),"T"=c("+","-")))
  )
         
  output$tab <- renderTable(tab(),rownames=TRUE, digits=6)
  output$tpr <- renderText({tab <- tab(); tab[2,1]/sum(tab[,1])})
  output$fpr <- renderText({tab <- tab(); tab[1,2]/sum(tab[,2])})
  output$mosaic <- renderPlot(mosaicplot(tab(),color=TRUE))  
}
shinyApp(ui=ui,server=server)





