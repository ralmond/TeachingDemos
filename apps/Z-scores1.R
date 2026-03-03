library(shiny)

ui <- fluidPage(
inputPanel(
  numericInput("mn", label = "Mean of X:",value=0,width=130),
  
  numericInput("sd", label = "Standard Deviation of X:",value=1,
              min = 0, width=130),
  numericInput("X", label = "x:",value=0, width=130)
),
mainPanel(
h3(textOutput("z")))
)

server <- function (input,output) {
  output$z <- renderText({
   paste("z = ",round((input$X-input$mn)/input$sd,3))
})
}
shinyApp(ui=ui,server=server)
