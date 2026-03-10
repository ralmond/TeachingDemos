library(shiny)
ui <- fluidPage(
inputPanel(
  numericInput("mn", label = "Mean Difference:",value=0,width=130),
  
  numericInput("sd", label = "Standard Deviation of Difference:",value=1,
              min = 0, width=130)
),
mainPanel(
  h3(textOutput("effect"))))

server <- function (input,output) {
  output$effect <-  renderText({
 paste("Effect size = ",round(input$mn/input$sd,2))
})
}
shinyApp(ui=ui,server=server)
