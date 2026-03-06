library(shiny)
N <- 100
ui1 <- fluidPage(
inputPanel(
  sliderInput("pad", label = "P(Member of Group A)",
              min = 0, max = 1, value = .5, step = 0.05),
  sliderInput("pyd", label = "P(Answered `yes`)",
              min = 0, max = 1, value = .5, step = 0.05),
  selectInput("OR",label="Odds Ratio",
              choices=c("1/4","1/3","1/2","2/3","1","3/2","2","3","4"), selected ="1")
),
mainPanel(
  tableOutput("table1"),
  plotOutput("mosaic1")))

server1 <- function (input,output) {

tabd <- reactive({
  pa <-input$pad
  py <-input$pyd
  OR <- eval(str2lang(input$OR))
  cat(pa,py,OR,"\n")
  if (OR == 1) {
    pay <- pa*py
  } else {
    S <- sqrt((1+(pa+py)*(OR-1))^2 + 4*OR*(1-OR)*pa*py)
    cat(S,"\n")
    pay <- (1+(pa+py)*(OR-1)-S)/2/(OR-1)
    cat(pay,"\n")
  }
  matrix(N*c(pay,(pa-pay),(py-pay),(1-py-pa+pay)),
        2,2, byrow=TRUE,
        dimnames=list(Group=c("A","B"),Answer=c("y","n")))
})
output$table1 <- renderTable(tabd(),rownames=TRUE, digits=1)
output$mosaic1 <- renderPlot(mosaicplot(tabd(),color=TRUE,main="Dependent data"))
}
shinyApp(ui=ui1,server=server1)

