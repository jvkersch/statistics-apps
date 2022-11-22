library(shiny)
library(ggplot2)

ui <- fluidPage(titlePanel("Growth curves"),
                sidebarLayout(
                  sidebarPanel(
                    h5("Gompertz"),
                    sliderInput(
                      "G_A",
                      "A:",
                      min = 0.1,
                      max = 5,
                      value = 3
                    ),
                    sliderInput(
                      "G_B",
                      "B:",
                      min = 0.1,
                      max = 10,
                      value = 1
                    ),
                    sliderInput(
                      "G_C",
                      "C:",
                      min = 0.1,
                      max = 8,
                      value = 1
                    ),
                    h5("Logistic"),
                    sliderInput(
                      "L_A",
                      "A:",
                      min = 0.1,
                      max = 5,
                      value = 3.5
                    ),
                    sliderInput(
                      "L_k",
                      "k:",
                      min = 0.1,
                      max = 8,
                      value = 1
                    ),
                    sliderInput(
                      "L_x0",
                      "x0:",
                      min = -4,
                      max = 4,
                      value = 0
                    )
                  ),
                  
                  mainPanel(plotOutput("distPlot"))
                ))

gompertz <- function(x, A, B, C) {
  A * exp(-B * exp(-C * x))
}

logistic <- function(x, A, k, x0) {
  A / (1 + exp(-k * (x - x0)))
}

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- seq(-5, 5, length.out = 100)
    y_g <- gompertz(x, input$G_A, input$G_B, input$G_C)
    y_l <- logistic(x, input$L_A, input$L_k, input$L_x0)
    # plot(x, y_g, type = "l", ylim = c(0, 5))
    # lines(x, y_l)
    df <- data.frame(x=x, y_g=y_g, y_l=y_l)
    ggplot(df) +
      geom_line(aes(x=x, y=y_g, color="red")) +
      geom_line(aes(x=x, y=y_l, color="lightblue")) +
      ylim(0, 5) +
      xlab("") +
      ylab("")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
