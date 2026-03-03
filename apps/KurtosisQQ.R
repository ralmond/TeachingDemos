library(lattice)
library(shiny)

distlist <-list(
platykurtic = list("Uniform"=runif,
  "Mixture of normals (different means)"=
    function (n)
      ifelse(runif(n)<.5,rnorm(n,-1),
               rnorm(n,1)),
  "beta(1.5,1.5" =  function (n)
    rbeta(n,1.5,1.5)),
leptokurtic = list("t(df=5)"=function(n) rt(n,5),
                "Mixture of normals (different sds)"=
                  function (n)
       ifelse(runif(n)<.25,rnorm(n,0,3),
               rnorm(n,0,1)),
       "Exponential" = rexp),
mesokurtic = list("normal"=rnorm, 
                  "Wiebul(2,2)"= function (n)
                    rweibull(n,2,2),
                  "Binomial(.45,10)"=function(n)
                    rbinom(n,10,.45)))

longnames <- c("Platykurtic (flat)"="platykurtic",
               "Leptokurtic (heavy tails)"="leptokurtic",
               "Mesokurtic (normal)"="mesokurtic")
## Initial draw, so that we have some starting values.
key <- 
{
  ## Randomly permute the types.  
  key <- sample(names(distlist),length(distlist))
  ## Label from A -- C (or whatever)  
  names(key) <- sapply(1L:length(key),
       function (i)
         intToUtf8(utf8ToInt("Z")-length(key)+i))
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
  plotOutput("QQs")),
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
  output$QQs <- renderPlot({
    ## Draw random data 
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
