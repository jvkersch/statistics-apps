library(shiny)
library(plotly)

# Load dataset
needles <- read.table("./data/needles.txt", header = T, sep = "\t", dec = ".")

create_model <- function(variables) {
  if (length(variables) == 0) {
    variables = c("1")
  }
  formula <- reformulate(termlabels = variables, response = "length")
  model <- model <- lm(formula, data = needles)
  return(model)
}

get_default <- function(coefs, name, default = 0) {
  if (is.na(coefs[name])) {
    default
  } else {
    coefs[name]
  }
}

get_length <- function(model, nitrogen, phosphor) {
  cs <- coef(model)
  0*nitrogen +
    cs["(Intercept)"] +
    get_default(cs, "nitrogen")*nitrogen +
    get_default(cs, "phosphor")*phosphor +
    get_default(cs, "I(nitrogen * phosphor)")*nitrogen*phosphor
}

create_surface <- function(model, grid) {
  get_length(model, grid$X, grid$Y)
}

create_nitrogen_traces <- function(model) {
  nitrogen <- seq(1.0, 3.0, length.out = 10)
  data.frame(
    nitrogen = nitrogen,
    trace1 = get_length(model, nitrogen, 0.15),
    trace2 = get_length(model, nitrogen, 0.20),
    trace3 = get_length(model, nitrogen, 0.25),
    trace4 = get_length(model, nitrogen, 0.30)
  )
}

create_nitrogen_effect <- function(model) {
  phosphor <- seq(0.15, 0.35, length.out = 10)

  coefs <- coef(model)
  beta_N <- get_default(coefs, "nitrogen")
  beta_NP <- get_default(coefs, "I(nitrogen * phosphor)")

  data.frame(
    phosphor = phosphor,
    effect = beta_N + beta_NP * phosphor
  )
}

ui <- fluidPage(
  headerPanel('Multiple Linear Regression'),
  sidebarPanel(
    fluidRow(
      verbatimTextOutput("Terms to include")),
    checkboxGroupInput(
      "variables",
      label=h3("Model terms"),
      choices=list(
        "Nitrogen"="nitrogen",
        "Phosphor"="phosphor",
        "Nitrogen × Phosphor"="I(nitrogen*phosphor)"),
      selected=c("nitrogen", "phosphor")
    ),
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot",
                         fluidRow(
                           plotlyOutput('plot', height = "100%")),
                         fluidRow(
                           splitLayout(
                             plotlyOutput('plot_nitrogen_traces', height = '100%'),
                             plotlyOutput('plot_nitrogen_effect', height = '100%'),
                             cellWidths = c('50%', '50%')
                           )
                         )),
                tabPanel("Summary", verbatimTextOutput("summary")))
  )
)

server <- function(input, output, session) {

  # Plotting grid
  nitrogen <- seq(1, 3, length.out = 50)
  phosphor <- seq(0.15, 0.40, length.out = 50)
  grid <- create_plotgrid(nitrogen, phosphor)

  model <- reactive(create_model(input$variables))
  Z <- reactive(create_surface(model(), grid))
  nitrogen_traces <- reactive(create_nitrogen_traces(model()))
  nitrogen_effect <- reactive(create_nitrogen_effect(model()))

  output$plot <- renderPlotly(
    plot1 <- subplot(
      plot_ly(x = nitrogen, y = phosphor, z = Z()) %>%
        add_surface(),
      plot_ly(x = needles$nitrogen,
              y = needles$phosphor,
              z = needles$length,
              marker = list(size = 5)) %>%
        add_markers()
    ) %>% layout(
      scene = list(
        xaxis = list(title = "Nitrogen"),
        yaxis = list(title = "Phosphor"),
        zaxis = list(title = "Length")
      )
    )
  )
  output$summary <- renderPrint(
    summary(model())
  )
  output$plot_nitrogen_traces <- renderPlotly(
    plot1 <- plot_ly(nitrogen_traces(), x = ~nitrogen, y = ~trace1,
                     type = 'scatter', mode = 'lines', name = 'P = 0.15') %>%
      add_trace(y = ~trace2, type = 'scatter', mode = 'lines', name = 'P = 0.20') %>%
      add_trace(y = ~trace3, type = 'scatter', mode = 'lines', name = 'P = 0.25') %>%
      add_trace(y = ~trace4, type = 'scatter', mode = 'lines', name = 'P = 0.30') %>%
      layout(
        title = "Nitrogen-length association",
        xaxis = list(title = "Nitrogen"),
        yaxis = list(title = "Length")))
  output$plot_nitrogen_effect <- renderPlotly(
    plot1 <- plot_ly(nitrogen_effect(), x = ~phosphor, y = ~effect,
                     type = 'scatter', mode = 'lines') %>%
      layout(
        title = "Effect of one unit of nitrogen",
        xaxis = list(title = "Phosphor"),
        yaxis = list(title = "Nitrogen effect")
      )
  )
}

shinyApp(ui, server)