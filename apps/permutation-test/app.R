library(shiny)

library(formattable)
library(scales)


ui <- fluidPage(titlePanel("Wilcoxon rank-sum test"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "distributions",
                      "Distributions",
                      choices=c(
                        "Separated"="separated",
                        "Distinct"="distinct",
                        "Interleaved"="interleaved")
                    ),
                    actionButton("step1", "Add 1 sample"),
                    br(),
                    actionButton("step100", "Add 100 samples"),
                    br(),
                    textOutput("nsamples"),
                    br(),
                    textOutput("pvalue")
                  ),
                  mainPanel(plotOutput("plot"))
                ))

# Data generators

make.data <- function(pop1, pop2) {
  total.pop <- c(pop1, pop2)
  s <- sort(total.pop, index.return = TRUE)
  list(data=c(pop1, pop2)[s$ix],
       mask=c(rep(TRUE, length(pop1)), 
              rep(FALSE, length(pop2)))[s$ix],
       n1=length(pop1),
       n2=length(pop2))
}

generate.data.separated <- function() {
  pop1 <- c(1, 2, 4, 4.8, 7.5)
  pop2 <- c(3.5, 6, 8, 9, 10)
  make.data(pop1, pop2)
}

generate.data.distinct <- function() {
  pop1 <- c(1, 2, 3, 4, 5)
  pop2 <- c(5.5, 6, 7, 9)
  make.data(pop1, pop2)
}

generate.data.interleaved <- function() {
  pop1 <- c(1, 3, 5, 7, 9)
  pop2 <- c(2, 4, 6, 8)
  make.data(pop1, pop2)
}

# Presenter code

prepare.colors <- function(groups) {
  ifelse(groups, "red", "blue")
}

apply.alpha <- function(colors, selected.indices) {
  a <- rep(0.2, length(colors))
  a[selected.indices] <- 1.0
  alpha(colors, a)
}

# W-statistic tools

run.w <- function(n1, n) {
  sample(seq(1, n), n1)
}

compute.w.statistic <- function(groups) {
  ranks <- seq(1, length(groups))
  sum(ranks[groups])
}

# Shiny code

server <- function(input, output) {
  
  ds <- generate.data.interleaved()

  total.pop <- ds$data
  n1 <- ds$n1
  n2 <- ds$n2
  n <- n1 + n2

  W <- compute.w.statistic(ds$mask)
  cat(file=stderr(), paste0("W = ", W, "\n"))
    
  v <- reactiveValues(ranks = c(),
                      selected = seq(1, length(total.pop)),
                      nsamples = 0,
                      pvalue = NA)
  
  observeEvent(input$step1, {
    s <- run.w(n1, n)
    v$ranks <- c(v$ranks, sum(s))
    v$selected <- s
    v$nsamples <- length(v$ranks)
    v$pvalue <- sum(v$ranks <= W) / length(v$ranks)
  })
  
  observeEvent(input$step100, {
    ss <- replicate(100, run.w(n1, n))
    v$ranks <- c(v$ranks, colSums(ss))
    v$selected <- ss[,ncol(ss)]
    v$nsamples <- length(v$ranks)
    v$pvalue <- sum(v$ranks <= W) / length(v$ranks)
  })
  
  output$nsamples <- renderText({
    paste0("Number of samples: ", v$nsamples)
  })
  
  output$pvalue <- renderText({
    paste0("Proportion smaller than W: ", percent(v$pvalue))
  })
  
  output$plot <- renderPlot({
    layout(matrix(
      c(1, 2, 2),
      nrow = 3,
      ncol = 1,
      byrow = TRUE
    ))

    y <- rep(0, length(total.pop))
    cols <- prepare.colors(ds$mask)
    cols <- apply.alpha(cols, v$selected)

    plot(
      total.pop,
      y,
      ylim = c(-1.5, 1.5),
      axes = FALSE,
      xlab = "",
      ylab = "",
      pch = 21,
      bg = cols,
      cex = 3
    )
    text(
      total.pop,
      y - 1.0,
      labels = seq(1, length(total.pop)),
      col = apply.alpha(rep("black", length(total.pop)), v$selected),
      cex = 2
    )
    
    if (length(v$ranks) > 0) {
      n <- n1 + n2
      
      xlim.min <- n1 * (n1 + 1) / 2 - 1
      xlim.max <- n * (n + 1) / 2 - n2 * (n2 + 1) / 2+ 1
      
      hist(
        v$ranks,
        main = "",
        xlab = "",
        ylab = "",
        yaxt = "n",
        freq = FALSE,
        xlim = c(xlim.min, xlim.max),
        cex.axis = 2
      )
      rug(jitter(v$ranks),
          lwd = 1,
          ticksize = 0.03)
      rug(v$ranks[[length(v$ranks)]],
          lwd = 10,
          ticksize = 0.05,
          col = "red")
      abline(v=W, col="blue", lwd=2)
    }
  })
}

shinyApp(ui = ui, server = server)
