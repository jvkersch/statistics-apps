library(plotly)
library(purrr)
library(ggplot2)
library(shiny)

compute_cook <- function(x, y) {
  m <- lm(y ~ x)
  d <- cooks.distance(m)
  df <- data.frame(
    n = as.factor(1:length(d)),
    d = d
  )
  return(df)
}

compute_leverage <- function(x, y) {
  m <- lm(y ~ x)
  d <- hatvalues(m)
  df <- data.frame(
    n = as.factor(1:length(d)),
    d = d
  )
  return(df)
}

compute_dfbetas <- function(x, y) {
  m <- lm(y ~ x)
  df <- cbind(
    n = as.factor(1:length(x)),
    as.data.frame(dfbetas(m))
  )
  return(df)
}

ui <- fluidPage(
  headerPanel('Regression outliers and leverage'),
  p("Drag the red point around to see the effect on the regression line and the outlier diagnostics. The red bar in the outlier diagnostics refers to the red point. The blue bars refers to the remaining 10 fixed data points."),
  fluidRow(
    column(12, plotlyOutput("regression"))
  ),
  fluidRow(
    column(4, plotOutput("leverage")),
    column(4, plotOutput("cooksdistance")),
    column(4, plotOutput("dfbetas"))
  )
)

server <- function(input, output, session) {
  set.seed(12345)  
  n <- 10
  X <- sort(3*runif(n))
  Y <- X + 0.3*rnorm(n)
  
  x_regr <- seq(-1, 4.5, length.out = n)
  
  n_obs <- n + 1
  
  outlier <- reactiveValues(x = 1.75, y = 2.25)
  
  # Simplified version of 
  # https://community.rstudio.com/t/update-scatter-in-plotly-by-dragging-shapes-shiny/44809/2
  observe({
    ed <- event_data("plotly_relayout")
    req(ed)
    
    if (grepl("shapes\\[0\\]", names(ed)[1])) {
      outlier$x <- ed[[1]]
      outlier$y <- ed[[2]]
    }
  })
  
  output$regression <- renderPlotly({
    # Regression model for data and outlier
    x <- c(X, outlier$x)
    y <- c(Y, outlier$y)
    m <- lm(y ~ x)
    y_regr <- predict(m, newdata=data.frame(x = x_regr))
    
    circles <- map(
      list(outlier),
      ~{
        list(
          type="circle",
          xanchor=.x$x,
          yanchor=.x$y,
          x0=-5, y0=-5,
          x1=5, y1=5,
          xsizemode="pixel",
          ysizemode="pixel",
          fillcolor="red",
          line=list(color = "transparent"),
          layer="below"
        )
      }
    )
    
    plot_ly(x = X, y = Y, text = 1:length(X)) %>%
      add_trace(x = x_regr, y = y_regr, mode = "lines") %>%
      add_markers() %>%
      add_text(textposition = "top right") %>%
      layout(shapes = circles, 
             showlegend = FALSE,
             xaxis = list(range=c(-1, 4.5)),
             yaxis = list(range=c(-1.5, 4.5))) %>%
      config(edits = list(shapePosition = TRUE))
  })
  
  output$leverage = renderPlot({
    x <- c(X, outlier$x)
    y <- c(Y, outlier$y)
    is_outlier <- as.factor(c(rep(0, n), 1))
    
    dist <- compute_leverage(x, y)
    
    ggplot(dist, aes(x = n, y = d, fill=is_outlier)) + 
      geom_bar(stat="identity") +
      scale_fill_manual(values = c("lightblue", "pink")) +
      theme(legend.position="none") +
      ggtitle("Leverage (hat values)") +
      labs(x = "", y = "") + 
      ylim(0, 1.0) +
      geom_hline(yintercept=2/length(x), linetype="dashed", color = "red")
  })
  
  output$cooksdistance = renderPlot({
    x <- c(X, outlier$x)
    y <- c(Y, outlier$y)
    is_outlier <- as.factor(c(rep(0, n), 1))
    
    dist <- compute_cook(x, y)
    
    ggplot(dist, aes(x = n, y = d, fill=is_outlier)) + 
      geom_bar(stat="identity") +
      scale_fill_manual(values = c("lightblue", "pink")) +
      theme(legend.position="none") +
      ggtitle("Cook's distance") +
      labs(x = "", y = "") + 
      ylim(0, qf(0.90, 2, n_obs-2)) + 
      geom_hline(yintercept=qf(0.50, 2, n_obs-2), linetype="dashed", color = "red")
  })
  
  output$dfbetas = renderPlot({
    x <- c(X, outlier$x)
    y <- c(Y, outlier$y)
    is_outlier <- as.factor(c(rep(0, n), 1))
    
    dist <- compute_dfbetas(x, y)
    
    ggplot(dist, aes(x = n, y = x, fill=is_outlier)) + 
      geom_bar(stat="identity") +
      scale_fill_manual(values = c("lightblue", "pink")) +
      theme(legend.position="none") +
      ggtitle("DFBETAs (Slope)") +
      labs(x = "", y = "") + 
      geom_hline(yintercept=c(1/sqrt(n_obs), -1/sqrt(n_obs)), 
                 linetype="dashed", color = "red") +
      coord_cartesian(ylim = c(-3/sqrt(n_obs), 3/sqrt(n_obs)))
  })
}


shinyApp(ui, server)