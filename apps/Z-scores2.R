library(shiny)

ui1 <- fluidPage(
inputPanel(
  numericInput("mny", label = "Mean of Y:",value=0,width=130),
  
  numericInput("sdy", label = "Standard Deviation of Y:",value=1,
              min = 0, width=130),
  numericInput("ZZ", label = "z:",value=0, width=130)
),
mainPanel(h3(textOutput("Y")))
)

server1 <- function (input,output) {
  output$Y <- renderText({
 paste("Y = ",round(input$ZZ*input$sdy+input$mny,3))
})
}
shinyApp(ui=ui1,server=server1)
