library(lattice)
library(shiny)
distlist <-list(
skewNeg = list("beta(8,2)"=function(n) rbeta(n,8,2),
                "normal with neg outliers"=function (n) {
                  ifelse(runif(n)<.05,rnorm(n,-3),rnorm(n))
                },
                "hypergeometric(975,25,100)" = 
                  function (n) rhyper(n,975,25,100)),
skewPos = list("gamma(3)"=function(n) rgamma(n,3),
                "normal with positive outliers"=function(n) {
                  ifelse(runif(n)<.05,rnorm(n,3),rnorm(n))
                },
                "lognormal"=function (n) rlnorm(n,0,.3)),
sym = list("normal"=rnorm, "uniform"=runif,
            "t(5 d.f.)"=function (n) rt(n,5)))
longnames <- c("Negatively Skewed"="skewNeg",
               "Positively Skewed"="skewPos",
               "Symmetric"="sym")
## Initial draw, so that we have some starting values.
key <- 
{
  ## Randomly permute the types.  
  key <- sample(names(distlist),length(distlist))
  ## Label from A -- C (or whatever)  
  names(key) <- sapply(1L:length(key),
       function (i)
         intToUtf8(utf8ToInt("A")-1L+i))
  key
}
kdist <- 
{
    # draw random distribution for each plot
    sapply(key, function (r)  
        sample(names(distlist[[r]]),1L))
}

ui <- fluidPage(
inputPanel(
  selectInput("nn", label = "Sample Size:",
              choices = c(50, 100, 500, 1000), selected = 500)),
mainPanel(
  plotOutput("QQplots")),
  h4("Which is which?"),
  p("Identify the skewness of each distribution."),
  do.call(inputPanel,
         lapply(names(key), function (k)
                selectInput(k, label=k,
                  choices=c(Unknown="unknown", 
                           longnames),
                  selected="unknown"))),
  h4("Answers:\n"),
  tableOutput("answers"))

server <- function (input,output) {
  output$QQplots <- renderPlot({
    ## Draw random data 
    print(key)
    kdat <- lapply(names(key), function (k) {
    x <-do.call(distlist[[key[k]]][[kdist[k]]],
                list(input$nn))
      scale(x,(min(x)+max(x))/2,(max(x)-min(x)))*100+50
  })
  names(kdat) <- names(key)
 kdat <- 
    data.frame(dat=do.call(c,kdat),
               group=rep(names(key),
                          each=input$nn))
  
  qqmath(~dat|group, data = kdat,
          layout=c(3,1),horizontal=FALSE,
         panel=function(x,...) {
           panel.qqmathline(x,...)
           panel.qqmath(x,...)
           })

})
  output$answers <- renderTable({
  answer <- sapply(names(key),
   function (k) {
      if (input[[k]]=="unknown") {
        "Make your selection.\n"
      } else {
        paste(ifelse(input[[k]]==key[k],
                     "Correct:", "Incorrect:"),
              "Distribution was",kdist[k],
               "(",
        names(longnames)[grep(key[k],longnames)],
               ")\n")
    }})
  names(answer) <- names(key)
  as.data.frame(answer)
}, colnames=FALSE,rownames=TRUE)
}
shinyApp(ui=ui,server=server)
