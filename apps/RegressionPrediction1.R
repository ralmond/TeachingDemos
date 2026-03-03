library(shiny)
ui1 <- fluidPage(
inputPanel(
  selectInput("alpha1", label = "Confidence Level:",
              choices = c(50, 68, 90, 95, 99), selected = 95)),
mainPanel(
  plotOutput("plot1")))
cars.fit <- lm (dist~speed,data=cars)

server1 <- function (input,output) {
  output$plot1 <- renderPlot({
  plot(dist~speed,data=cars,type="n",xlab="Speed (mph)",   ylab="Stopping Distance (ft)",ylim=c(-25,150))
 pred1 <- predict(cars.fit,data.frame(speed=1:25),interval="prediction",
          level=as.numeric(input$alpha1)/100)
 # Color the negative predictions.
 crossi <- max(which(pred1[,"lwr"]<0))
 crossl <- pred1[crossi+1,"lwr"] -pred1[crossi,"lwr"]
 crossx <- -pred1[crossi,"lwr"]/crossl
 polygon(c(1,1:crossi,crossx),c(0,pred1[1:crossi,"lwr"],0),col="cyan")
  abline(h=0)
  abline(cars.fit)
  points(cars$speed,cars$dist)
 lines(1:25,pred1[,"upr"],lty=2)
 lines(1:25,pred1[,"lwr"],lty=2)
})}

shinyApp(ui=ui1,server=server1)
