library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The density function as a limit of histograms"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of samples:",
                        min = 2,
                        max = 1000,
                        value = 50),
            checkboxInput("density", "Show density (approximate)", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(1234)
  n <- 100000
  LB <- 85
  UB <- 150
  samples <- 100 + 5 * (rlnorm(n, sdlog = 1.2) + rnorm(n, 0.1))
  samples <- samples[(samples > LB) & (samples < UB)]
  dens <- density(samples)
  
  output$distPlot <- renderPlot({
    n <- input$n
    hist(
      samples[1:n],
      freq = F,
      breaks = 50,
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = paste0("Number of samples: ", n)
    )
    if (input$density) {
      lines(dens$x, dens$y, col=2, lwd=3)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
