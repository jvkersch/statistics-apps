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
  
  n_obs <- n + 1
  
  outlier <- reactiveValues(x = 2.75, y = 0.0)
  
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
      add_markers() %>%
      add_text(textposition = "top right") %>%
      layout(shapes = circles, 
             showlegend = FALSE,
             xaxis = list(range=c(0, 3.5)),
             yaxis = list(range=c(-0.5, 3.5))) %>%
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
    
    # m <- lm(y ~ x)
    # plot(1, type="n", xlab="", ylab="", xlim=c(1, n+1), ylim=c(0, 1))
    # segments(1:(n+1), 0, 1:(n+1), hatvalues(m)) 
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