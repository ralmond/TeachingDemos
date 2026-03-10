library(shiny)
N <- 100
ui <- fluidPage(
inputPanel(
  sliderInput("pa", label = "P(Member of Group A)",
              min = 0, max = 1, value = .5, step = 0.05),
  sliderInput("py", label = "P(Answered `yes`)",
              min = 0, max = 1, value = .5, step = 0.05)
),
mainPanel(
  tableOutput("table"),
  plotOutput("mosaic")))

server <- function (input,output) {

tabi <- reactive(
  matrix(N*c(input$pa*input$py,
           input$pa*(1-input$py),
           (1-input$pa)*input$py,
           (1-input$pa)*(1-input$py)),2,2,
         byrow=TRUE,
         dimnames=list(Group=c("A","B"),Answer=c("y","n")))
)
output$table <- renderTable(tabi(),rownames=TRUE, digits=1)
output$mosaic <- renderPlot(mosaicplot(tabi()),color=TRUE,main="Independent data")
}
shinyApp(ui=ui,server=server)
