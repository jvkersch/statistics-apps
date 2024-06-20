library(shiny)
library(ggplot2)

# Cheat a little: randomly sample data ahead of time and then progressively reveal it.

create_data <- function(n = 10000) {
  x <- runif(n, min = -1.0, max = 1.0)
  y <- runif(n, min = -1.0, max = 1.0)
  is_inside <- x ^ 2 + y ^ 2 < 1
  n_inside <- cumsum(is_inside)
  n_total <- seq(1, n)
  data.frame(
    x = x,
    y = y,
    is_inside = is_inside,
    n_inside = n_inside,
    n_total = n_total,
    fraction_inside = n_inside / n_total
  )
}

# UI
ui <- fluidPage(#
  titlePanel("Monte Carlo"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_one", "+1"),
      actionButton("add_hundred", "+100"),
      actionButton("reset", "Reset"),
      h3("Instructions"),
      p(
        paste0(
          "Use the +1 button to add a random point in the rectangle ",
          "[-1,1] x [-1, 1]. Points that fall inside the unit circle ",
          "are colored in green, and those outside are colored in red. The ",
          "+100 button adds 100 points at a time."
          )),
      p(
        paste0(
          "The line plot shows the fraction of points that fall inside the ",
          "unit circle. As more and more points are added, this fraction ",
          "converges to a number slightly above 0.75. What is that number?"
        ))
    ),
    mainPanel(plotOutput("circle_plot"), plotOutput("fraction_plot"))
  ))

# Server
server <- function(input, output) {
  data <- reactiveVal(create_data())
  n <- reactiveVal(0)

  output$circle_plot <- renderPlot({
    circle_data <- data.frame(x = cos(seq(0, 2 * pi, length.out = 100)),
                              y = sin(seq(0, 2 * pi, length.out = 100)))
    ggplot(circle_data, aes(x, y)) +
      geom_path() +
      xlim(-1.2, 1.2) +
      ylim(-1.2, 1.2) +
      geom_point(data = head(data(), n = n()), aes(x, y, color = is_inside)) +
      coord_fixed(ratio = 1) +
      theme(legend.position = "none", text = element_text(size = 20))
  })

  output$fraction_plot <- renderPlot({
    ggplot(head(data(), n = n()), aes(n_total, fraction_inside)) +
      geom_line() +
      xlab("Points Added") +
      ylab("Fraction Inside Circle") +
      ylim(0, 1) +
      geom_hline(yintercept = pi / 4, linetype = "dashed") +
      theme(text = element_text(size = 20))
  })

  observeEvent(input$add_one, {
    n(n() + 1)
  })
  observeEvent(input$add_hundred, {
    n(n() + 100)
  })
  observeEvent(input$reset, {
    data(create_data())
    n(0)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
