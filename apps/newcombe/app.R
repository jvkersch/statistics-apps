library(ggplot2)
library(shiny)
library(tidyr)
library(dplyr)
library(purrr)

source("auc_ci.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Newcombe (2006) Confidence Intervals for AUC"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("auc",
                        "Area Under the Curve (AUC)",
                        min = 0.0,
                        max = 1.0,
                        value = 0.5),
            sliderInput("events",
                        "Number of events",
                        min = 2,
                        max = 100,
                        value = 10),
            sliderInput("nonevents",
                        "Number of non-events",
                        min = 2,
                        max = 100,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("forest_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$forest_plot <- renderPlot({
    fs <- c(newcombe_2, newcombe_3, newcombe_4, newcombe_5)
    df <- bind_rows(
      map(fs, \(f) f(input$auc, input$events, input$nonevents))
    )
    df$name <- name <- factor(c(2, 3, 4, 5))
    df$auc <- input$auc
    ggplot(df) +
      geom_segment(aes(y = name, yend = name,
                       x = lower, xend = upper,
                       color = name), linewidth = 2) +
      geom_point(aes(x = auc, y = name, color = name), size = 5) +
      xlim(c(-0.1, 1.1)) +
      geom_vline(xintercept = c(0, 1), linetype = "dashed", color = "gray") +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_discrete(breaks = name,
                         labels = paste0("Newcombe ", name)) +
      xlab("Area Under the Curve (AUC)") + ylab(NULL) +
      theme_minimal() +
      theme(legend.position = "none", text=element_text(size=18))

  })
}

# Run the application
shinyApp(ui = ui, server = server)
