library(shiny)
ui <- fluidPage(
  inputPanel(
    selectInput("n_breaks", label = "Number of bins:",
                choices = c(10, 20, 35, 50), selected = 20),
  
    sliderInput("bw_adjust", label = "Bandwidth adjustment:",
                min = 0.2, max = 2, value = 1, step = 0.2)
  ),
  mainPanel(plotOutput("plot")))
  
server <- function(input,output) {
  output$plot <- renderPlot({
      hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
           xlab = "Duration (minutes)", main = "Geyser eruption duration")
  
      dens <- density(faithful$eruptions, adjust = input$bw_adjust)
      lines(dens, col = "blue")
  })
}
shinyApp(ui=ui,server=server)

