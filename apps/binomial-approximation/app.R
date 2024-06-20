library(shiny)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(
    "Approximating the binomial distribution with a normal distribution"
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "n",
        "Number of trials:",
        min = 1,
        max = 50,
        value = 30
      ),
      sliderInput(
        "p",
        "Probability of success:",
        min = 0.01,
        max = 0.99,
        value = 0.5
      ),
      checkboxInput("truncate", "Truncate x-axis", TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("error"),
      textOutput("condition")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    p <- input$p
    n <- input$n

    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))

    xs <- seq(0, n)
    ys.binomial <- dbinom(xs, n, p)

    if (input$truncate) {
      # threshold so that we focus on the bulk of the distribution
      mask <- ys.binomial > 0.01
      xs <- xs[mask]
      ys.binomial <- ys.binomial[mask]
    }

    plot(
      NULL,
      xlim = c(min(xs), max(xs)),
      ylim = c(0, max(ys.binomial)),
      ylab = "",
      xlab = "",
      xaxt = "n",
      bty = "L",
      main = ""
    )
    axis(1, at = xs)
    segments(xs,
             rep(0, n),
             xs,
             ys.binomial,
             lwd = 9,
             col = "gray")

    xs.norm <- seq(min(xs), max(xs), length.out = 100)
    ys.norm <- dnorm(xs.norm, mu, sigma)

    lines(xs.norm, ys.norm, col = 2, lwd = 3)

  })

  output$error <- renderText({
    p <- input$p
    n <- input$n

    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))

    xs <- seq(0, n)
    ys.binomial <- dbinom(xs, n, p)
    ys.norm <- dnorm(xs, mu, sigma)

    diff <- abs(ys.binomial - ys.norm)
    i <- which.max(diff)
    error <- diff[i] / ys.binomial[i]

    paste0(
      "Maximal relative error: ",
      round(error, 2),
      " (",
      label_percent(accuracy = 0.1)(error),
      ")"
    )
  })

  output$condition <- renderText({
    p <- input$p
    n <- input$n
    paste0("Condition for approximation: ", round(n * p * (1 - p), 1))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
