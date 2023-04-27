library(shiny)
library(deSolve)

our_data <- read.csv("vanrolleghem_our.csv")
times <- seq(0, 45, by = 0.1)

monod_rhs <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    list(c(-alpha*s/(beta + s)))
  })
}

monod_solve <- function(times, s0, alpha, beta) {
  state <- c(s=s0)
  parameters <- c(alpha=alpha, beta=beta)
  sol <- ode(y = state, times = times, parms = parameters, func = monod_rhs)
  ds_dt <- sapply(sol[,"s"], function(s) {
    monod_rhs(0.0, c(s=s), parameters = parameters)[[1]]
  })
  return(list(s=sol[,"s"], ds_dt=ds_dt))
}

single_monod <- function(times, alpha, beta, s0) {
  sol <- monod_solve(times, s0, alpha, beta)
  alpha * sol$s / (beta + sol$s)
}


ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    h3("Monod 1"),
    sliderInput(
      "alpha1",
      "alpha:",
      min = 0,
      max = 2,
      value = 0.344,
      step = 0.05
    ),
    sliderInput(
      "beta1",
      "beta:",
      min = 0,
      max = 10,
      value = 1.67,
      step = 0.05
    ),
    sliderInput(
      "s1",
      "s0:",
      min = 0,
      max = 6,
      value = 4.22,
      step = 0.05
    ),
    h3("Monod 2"),
    sliderInput(
      "alpha2",
      "alpha:",
      min = 0,
      max = 2,
      value = 0.47,
      step = 0.05
    ),
    sliderInput(
      "beta2",
      "beta:",
      min = 0,
      max = 3,
      value = 0.13,
      step = 0.05
    ),
    sliderInput(
      "s2",
      "s0:",
      min = 0,
      max = 6,
      value = 4.09,
      step = 0.05
    )
  ),
  mainPanel(
    plotOutput("distPlot"),
    withMathJax(),
    p(
      'Fit the double Monod model to the data shown in the plot. The sliders control
      the parameters of each single Monod model, and the double Monod model is the
      sum of the two single Monod models. The expression for a single Monod model is
      described by the following ODE: $$\\frac{d S}{d t} = - \\frac{\\alpha S}{\\beta + S}.$$
      The parameters of this model are \\(\\alpha\\), \\(\\beta\\), and the
      inital condition \\(S_0\\).')
  )
))

server <- function(input, output) {
  output$distPlot <- renderPlot({
    par(cex = 1.5)
    plot(
      our_data[, 1],
      our_data[, 2],
      ylim = c(0, 1),
      pch = 20,
      cex = 0.5,
      xlab = "Time",
      ylab = "S(t)"
    )
    m1 <- single_monod(times, input$alpha1, input$beta1, input$s1)
    m2 <- single_monod(times, input$alpha2, input$beta2, input$s2)
    lines(times, m1 + m2, lty = 1, lwd = 2)
    lines(times, m1, lty = 2, lwd = 2)
    lines(times, m2, lty = 3, lwd = 2)
    legend("topright", legend = c("Combined", "Monod 1", "Monod 2"),
           lty = 1:3, lwd = 2)
  })

}

shinyApp(ui, server)
