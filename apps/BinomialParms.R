---
title: "Binomial Parameters"
author: "Russell Almond"
date: "September 1, 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

The [binomial distribution](https://en.wikipedia.org/wiki/Binomial_distribution) can be thought of as a number of draws, $n$, from an urn with a proportion $p$, of black balls.

#| standalone: true
#| viewerHeight: 600
library(shiny)
library(ggplot2)
ui <- fluidPage(
inputPanel(
  sliderInput("n", label = "Number of draws:",
              min=0, max=100, value=10, step=1),
  
  sliderInput("p", label = "Probability of success:",
              min = 0, max = 1, value = .6, step = 0.01)
),
mainPanel(
  plotOutput("bincurve")))

server <- function (input,output) {
  output$bincurve <- renderPlot({
  n <- as.numeric(input$n)
  p <- as.numeric(input$p)
  dat <- data.frame(x=0:n,y=dbinom(0:n,n,p))
  ggplot(dat,aes(x,y)) +geom_col()  

})
}
shinyApp(ui=ui,server=server)
