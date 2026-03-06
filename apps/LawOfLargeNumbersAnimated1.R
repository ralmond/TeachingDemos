library(shiny)
library(plotly)
library(patchwork)
library(tidyverse)
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
ui <- fluidPage(
inputPanel(
  selectInput("N", label = "Maximum Sample Size:",
              choices = c(50, 100, 200, 500, 1000), selected = 500),
  
  sliderInput("p", label = "Probability of event (p)",
              min = 0, max = 1, value = .5, step = 0.01),
  sliderInput("delta", label = "Distance of reference line from target (delta)",
              min = 0, max = .1, value = .05, step = 0.005)

),
mainPanel(
  plotlyOutput("plot1")))

server <- function (input,output) {
  output$plot1 <- renderPlotly({
  n <- 1:input$N
  x <- runif(input$N) < input$p
  pn <- cumsum(x)/n
  datalist <- lapply(n,function(nn)
    data.frame(n=1:nn,pn=pn[1:nn],f=nn))
  data <- dplyr::bind_rows(datalist)
  target <- input$p
  bounds <- input$p+c(-1,1)*input$delta
  fig <- 
      ggplot(data,aes(x=n,y=pn, frame=f)) +
      geom_line() +
      xlab("Number of Trials") +
      ylab("Proportion Success") +
      geom_hline(aes(yintercept=target,col="target")) +
      geom_hline(aes(yintercept=bounds[1],col="bound")) +
      geom_hline(aes(yintercept=bounds[2],col="bound")) + 
      labs(col="Target Lines") +
      scale_color_manual(values=c(target="blue",bound="skyblue"))
  
  ggplotly(fig) %>%  animation_opts(frame=100,transition=0,redraw=FALSE)
})}
shinyApp(ui=ui,server=server)
