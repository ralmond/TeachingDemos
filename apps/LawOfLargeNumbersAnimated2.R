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
nmax <- 1000
rdist <- list(Normal=rnorm, Exponential = rexp, 
              Gamma = function(n) rgamma(n,3),
              "T" = function(n) rt(n,3))
pdist <- list(Normal=pnorm, Exponential = pexp, 
              Gamma = function(q) pgamma(q,3),
              "T" = function(q) pt(q,3))
ddist <- list(Normal=dnorm, Exponential = dexp, 
              Gamma = function(x) dgamma(x,3),
              "T" = function(x) dt(x,3))

ui1 <- fluidPage(
inputPanel(
  selectInput("dist",label="Distribution Type",
              choices=c("Normal","Exponential","Gamma","T"),
              selected="Normal"),
   selectInput("NN", label = "Maximum Sample Size:",
              choices = c(50, 100, 200, 500, 1000), selected = 500),
),
mainPanel(
  plotlyOutput("erf"),
  plotlyOutput("histplot")))

server1 <- function (input,output) {

cumdat <- reactive({
  NN <- input$NN
  XX <- do.call(rdist[[input$dist]],list(NN))
  bind_rows(
      lapply(25:NN,function(i)
        data.frame(x=sort(XX[1:i]),Fn=(1:i)/i,f=i)))
})

output$erf <- renderPlotly({
  erfplot <- ggplot(cumdat(),aes(x,y=Fn,frame=f)) + geom_point()+stat_function(fun=pdist[[input$dist]],geom = "line",col="red") +  labs(title="Actual vs Empirical Distribution Function")
  
 
 ggplotly(erfplot) %>% animation_opts(frame=100) 
})

output$histplot <- renderPlotly({
 histplot <- ggplot(cumdat(),aes(x,frame=f)) + geom_histogram(aes(y=..density..),binwidth=.25, position="identity") +
 stat_function(fun=ddist[[input$dist]],geom="line",col="red") + labs(
       title="Actual vs Empirical Density Function")

 ggplotly(histplot) %>% animation_opts(frame=100)
})
}
shinyApp(ui=ui1,server=server1)

