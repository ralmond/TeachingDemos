library(shiny)
ui <- fluidPage(
inputPanel(
  numericInput("baserate", label = "Base Rate (Pr(D=Y)):", value=.0000458,width=100),
 numericInput("sensitivity", label = "Sensitivity (Pr(T=+|D=Y)):", value=.972,width=100),
numericInput("specificity", label = "Specificity (Pr(T=-|D=N)):", value=.92,width=100)
),
mainPanel(
  tableOuput("tab"),
  h2("Calculating true positive and false positive"),
  markdown("One way to calculate this is to use *Bayes' theorem*. However, from the table above, it is easy to calculate the true positive and false positive rates. We now just look at the columns of the table."),
  p(strong("False Positive Rate"),"Pr(D=N|T=+)",textOutput("tpr")),
  p(strong("False Negative Rate"),"Pr(D=Y|T=-)",textOutput("fpr")),
  h3("What is going on???"),
  p("That false positive rate seems very high. What is really going on? The root cause is that as of this writing (Mar 22, 2020) COVID-19 is still pretty rare. So although getting a false positive is rare, actually having COVID-19 is much rarer. The following picture might help:"),
  plotOutput("mosaic")
))

server <- function (input,output) {

  tab <- reactive(
    matrix(c(input$baserate*input$sensitivity,
           input$baserate*(1-input$sensitivity),
           (1-input$baserate)*(1-input$specificity),
           (1-input$baserate)*input$specificity),2,2,
         byrow=TRUE,
         dimnames=list(D=c("Y","N"),"T"=c("+","-")))
  )
         
  output$tab <- renderTable(tab(),rownames=TRUE, digits=6)
  output$tpr <- renderText({tab <- tab(); tab[2,1]/sum(tab[,1])})
  output$fpr <- renderText({tab <- tab(); tab[1,2]/sum(tab[,2])})
  output$mosaic <- renderPlot(mosaicplot(tab()),color=TRUE)  
}
shinyApp(ui=ui,server=server)





The thin bar on the left represents people who have COVID-19. There are still (fortunately) very few of them. The bar on top represents the false positives, fortunately, there are still a lot more of them than the true positives, so true positives are still rare. (May it always be so).

On the other hand, the false negative rate is very comforting. It means that if you test negative, you can be pretty sure that it is safe for you to be around other people (especially the old or sick).

## Sensitivity Analysis

Don't forget that these base rates are underestimates. There is currently a shortage of tests, so these are only cases that have actually be able to be tested. Also, symptoms can take up to 3 days to appear, so some people who have it, probably don't even know that they should ask to be tested. The *actual* infection rate could be 10 or more times as high as the *known* infection rate.

Also, there are various risk factors which should be added to the base rate. If the person being tested has traveled lately to an area with a higher rate, the base rate should go up. So too if the person has a fever or other symptoms of the virus.

So, play around with the base rate. Play with the sensitivity and specificity? How does this change? This will help you get a better feel for how the rare disease problem works.

Finally, don't forget that this thing grows exponentially fast (that is why it is a pandemic). This number could be go up very quickly. [Here is an explanation.](https://youtu.be/Kas0tIxDvrg). ( As an aside, this is the kind of thing we would analyze on the log scale.)

## How would this test be used.

Actually, the most interesting thing about the CoronaCheck kit is that it only takes 15 minutes. This is great considering the older test takes 3 days. So assuming BioResponse can produce these quickly (or that other vendors come online with similar tests), these can be used for screening (say health care workers, or other first responders), as well as people presenting with other symptoms or having recently traveled.

If these people test positive on the quick screening test, they should be isolated and possibly a more sensitive (and probably time consuming) test be given. If they test negative, then they can be cleared to go about their normal activity. I'm sure this is how this test will be used.

Another factor is that doctors are simply not giving out tests unless there are other risk factors. I was in my doctors office for my daughter's physical and talking to the nurse. She said that there was a woman who was tired (needs more sleep?) and congested (this is Tallahassee in March, the trees are raining pollen), but no fever. The nurse had to explain that she didn't have enough test to give out unless there were more symptoms (particularly a fever). This will change as our testing capacity gets better (last I looked, Mar 20, the US was still doing only about 1/2 the number of test per capita as South Korea.

## Don't break lockdown/self-isolation

*Don't panic, but do not be complacent either.*

Some of you reading this will be in official lockdown. Others will be under a self-distancing protocol. This is still extremely important as (1) the base rate will rise over time, probably quite quickly and (2) the disease takes up to 3 days to get started and the symptoms might appear like a common cold (Novel Coronavirus 19 is in fact an uncommon cold). You might have it and not know it yet. If you break the self-distancing protocol, you could be another Typhoid Mary spreading sickness and misery all around you.

Oh, and congrats to BioResponse on their breakthrough. I can't judge the quality of the numbers from just a press release, but if they really can make that number of tests, that would be a big help. I hope lots of other biotech companies are working on this problem, too.

Stay healthy. Keep your distance. Wash your hands, and obey the local health authorities. Lets make sure we keep that base rate (i.e., the infection rate) low.
