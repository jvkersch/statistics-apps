library(shiny)
library(deSolve)

our.data <- read.csv("vanrolleghem_our.csv")
#our.data <- matrix(c(5, 15, 0.85, 0.3), ncol = 2)
times <- seq(0, 45, by = 0.1)

monod.rhs <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    list(c(-alpha*s/(beta + s)))
  })
}

monod.solve <- function(times, s0, alpha, beta) {
  state <- c(s=s0)
  parameters <- c(alpha=alpha, beta=beta)
  sol <- ode(y = state, times = times, parms = parameters, func = monod.rhs)
  ds.dt <- sapply(sol[,"s"], function(s) {
    monod.rhs(0.0, c(s=s), parameters = parameters)[[1]]
  })
  return(list(s=sol[,"s"], ds.dt=ds.dt))
}

our.single.monod <- function(times, alpha, beta, s0) {
  sol <- monod.solve(times, s0, alpha, beta)
  alpha * sol$s / (beta + sol$s)
}


ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    sliderInput(
      "alpha1",
      "alpha1:",
      min = 0,
      max = 10,
      value = 0.344,
      step = 0.05
    ),
    sliderInput(
      "beta1",
      "beta1:",
      min = 0,
      max = 10,
      value = 1.67,
      step = 0.05
    ),
    sliderInput(
      "s1",
      "s1:",
      min = 0,
      max = 6,
      value = 4.22,
      step = 0.05
    ),
    sliderInput(
      "alpha2",
      "alpha2:",
      min = 0,
      max = 3,
      value = 0.47,
      step = 0.05
    ),
    sliderInput(
      "beta2",
      "beta2:",
      min = 0,
      max = 3,
      value = 0.13,
      step = 0.05
    ),
    sliderInput(
      "s2",
      "s2:",
      min = 0,
      max = 6,
      value = 4.09,
      step = 0.05
    )
  ),
  mainPanel(plotOutput("distPlot"))
))

server <- function(input, output) {
  output$distPlot <- renderPlot({
    plot(
      our.data[, 1],
      our.data[, 2],
      ylim = c(0, 1),
      pch = 20,
      cex = 0.5
    )
    m1 <- our.single.monod(times, input$alpha1, input$beta1, input$s1)
    m2 <- our.single.monod(times, input$alpha2, input$beta2, input$s2)
    lines(times, m1, lty = 2)
    lines(times, m2, lty = 3)
    lines(times, m1+m2)
  })

}

shinyApp(ui, server)
