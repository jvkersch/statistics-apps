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

create_surface <- function(model, grid) {
  cs <- coef(model)
  Z <- 0*grid$X + cs["(Intercept)"] 
  if (!is.na(cs["nitrogen"])) {
    Z <- Z + cs["nitrogen"]*grid$X
  }
  if (!is.na(cs["phosphor"])) {
    Z <- Z + cs["phosphor"]*grid$Y
  }
  if (!is.na(cs["I(nitrogen * phosphor)"])) {
    Z <- Z + cs["I(nitrogen * phosphor)"]*grid$X*grid$Y
  }
  return(Z)
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
                tabPanel("Plot", plotlyOutput('plot', height = "100%")),
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
  
  output$plot <- renderPlotly(
    plot1 <- subplot(
      plot_ly(x = nitrogen, y = phosphor, z = Z()) %>% add_surface(
        contours = list(
          z = list(
            show = TRUE,
            usecolormap = TRUE,
            highlightcolor="#ff0000",
            project=list(z=TRUE)
          )
        )
      ),
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
}

shinyApp(ui, server)