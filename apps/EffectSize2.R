library(shiny)
ui1 <- fluidPage(
inputPanel(
  fluidRow(column(6,"Statistics for Group 1 (experimental/focal)",
                    numericInput("mn1", label = "Mean:",value=0,width=230),
                    numericInput("sd1", label = "SD:",value=1,min = 0, width=230),
                    numericInput("N1",label="N:",value=5,min=0,step=1,width=230)),
           column(6,"Statistics for Group 2 (control/reference)",
                    numericInput("mn2", label = "Mean:",value=0,width=230),
                    numericInput("sd2", label = "SD:",value=1,min = 0, width=230),
                    numericInput("N2",label="N:",value=5,min=0,step=1,width=230))
  )
                    
),
mainPanel(
  h3(textOutput("effect2"))))

server1 <- function (input,output) {
  output$effect2 <- renderText({
  mdiff <- input$mn1-input$mn2
  spooled <- sqrt(((input$N1-1)*input$sd1^2+
                   (input$N2-1)*input$sd2^2)/
    (input$N1+input$N2-2))
 paste("Mean difference = ", round(mdiff,2), ", Pooled SD = ", round(spooled,3),
      ", Effect size = ",round(mdiff/spooled,2))
})
}
shinyApp(ui=ui1,server=server1)
