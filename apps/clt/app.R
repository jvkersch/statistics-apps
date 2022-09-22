#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(rmutil)

ui <- fluidPage(

    titlePanel("The Central Limit Theorem"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distribution",
                        "Population distribution",
                        choices = c("Log-normal", "Normal", "Levy")),
            sliderInput("n",
                        "Number of data points per mean:",
                        min = 1,
                        max = 200,
                        value = 30)
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

prepare.lognormal <- function() {
  return(list(samples=rlnorm,
              pdf=dlnorm,
              xmin=0.05,
              xmax=3.0,
              mean=1.65,
              stdev=2.16))
}

prepare.normal <- function() {
  return(list(samples=rnorm,
              pdf=dnorm,
              xmin=-3.5,
              xmax=3.5,
              mean=0,
              stdev=1))
}

prepare.levy <- function() {
  return(list(samples=rlevy,
              pdf=dlevy,
              xmin=0.1,
              xmax=4,
              mean=NA,
              stdev=NA))
}

server <- function(input, output) {

    output$distPlot <- renderPlot({
      n <- input$n

      if (input$distribution == "Log-normal") {      
        distdata <- prepare.lognormal()
      } else if (input$distribution == "Normal") {
        distdata <- prepare.normal()
      } else if (input$distribution == "Levy") {
        distdata <- prepare.levy()
      }
        
      sample.means <- rowMeans(matrix(distdata$samples(n*1000), ncol = n))

      par(mfrow=c(1, 3), cex=1.5)
      
      # Population distribution (doesn't change)
      xmin <- distdata$xmin
      xmax <- distdata$xmax
      xs <- seq(xmin, xmax, length.out = 100)
      ys <- distdata$pdf(xs)
      plot(xs, ys, type="l", main = "Population distribution", xlab="", ylab="")
      abline(v=distdata$mean)
      
      # Histogram of sample means
      xlim.min <- min(xmin, quantile(sample.means, probs = 0.01))
      xlim.max <- max(xmax, quantile(sample.means, probs = 0.99))
      param <- fitdistr(sample.means, "normal")$estimate
      hist(sample.means, main = sprintf("Sample mean (n = %d)", n), 
           xlab = "", ylab = "", xlim = c(xlim.min, xlim.max), probability = TRUE)
      curve(dnorm(x, param[1], param[2]), col = 2, lwd = 3, add = T)

      # QQ plot of sample means
      qqnorm(sample.means, main = sprintf("Q-Q plot (n = %d)", n))
      qqline(sample.means)
    })
}

shinyApp(ui = ui, server = server)