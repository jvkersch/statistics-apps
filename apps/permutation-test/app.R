library(shiny)

library(formattable)
library(scales)


ui <- fluidPage(titlePanel("Wilcoxon rank-sum test"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "distribution",
                      "Distributions",
                      choices=c(
                        "Separated"="separated",
                        "Distinct"="distinct",
                        "Interleaved"="interleaved")
                    ),
                    actionButton("step1", "Add 1 permutation"),
                    br(),
                    actionButton("step100", "Add 100 permutations"),
                    br(),
                    textOutput("wdata"),
                    textOutput("nsamples"),
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

# TODO: split off plotting code, clean up huge mess with reactives and streamline variable names

server <- function(input, output) {

  v <- reactiveValues()
  observeEvent(input$distribution, {


    bundle <- switch(
      input$distribution,
      "separated"=generate.data.separated,
      "distinct"=generate.data.distinct,
      "interleaved"=generate.data.interleaved
    )()

    # Initalize reactive context

    # TODO: simply make bundle into a single reactive
    v$total.pop <- bundle$data
    v$groups <- bundle$mask
    v$n1 <- bundle$n1
    v$n2 <- bundle$n2
    v$n <- bundle$n1 + bundle$n2
    v$W <- compute.w.statistic(bundle$mask)

    # TODO split off state
    v$ranks = c()
    v$selected = seq(1, v$n)
    v$nsamples = 0
    v$pvalue = 0
  })

  observeEvent(input$step1, {
    s <- run.w(v$n1, v$n)
    v$ranks <- c(v$ranks, sum(s))
    v$selected <- s
    v$nsamples <- length(v$ranks)
    v$pvalue <- sum(v$ranks <= v$W) / max(1., length(v$ranks))
  })

  observeEvent(input$step100, {
    ss <- replicate(100, run.w(v$n1, v$n))
    v$ranks <- c(v$ranks, colSums(ss))
    v$selected <- ss[,ncol(ss)]
    v$nsamples <- length(v$ranks)
    v$pvalue <- sum(v$ranks <= v$W) / max(1., length(v$ranks))
  })

  output$wdata <- renderText({
    paste0("W-statistic of data: ", v$W)
  })

  output$nsamples <- renderText({
    paste0("Number of samples: ", v$nsamples)
  })

  output$pvalue <- renderText({
    paste0("Proportion ≤ W: ", percent(v$pvalue))
  })

  output$plot <- renderPlot({
    layout(matrix(
      c(1, 2, 2),
      nrow = 3,
      ncol = 1,
      byrow = TRUE
    ))

    y <- rep(0, length(v$total.pop))
    cols <- prepare.colors(v$groups)
    cols <- apply.alpha(cols, v$selected)

    plot(
      v$total.pop,
      y,
      ylim = c(-1.5, 1.5),
      axes = FALSE,
      xlab = "",
      ylab = "",
      pch = ifelse(v$groups, 21, 22),
      bg = cols,
      cex = 3
    )
    text(
      v$total.pop,
      y - 1.0,
      labels = seq(1, length(v$total.pop)),
      col = apply.alpha(rep("black", length(v$total.pop)), v$selected),
      cex = 2
    )

    if (length(v$ranks) > 0) {
      n <- v$n1 + v$n2
      n1 <- v$n1
      n2 <- v$n2

      xlim.min <- n1 * (n1 + 1) / 2 - 1
      xlim.max <- n * (n + 1) / 2 - n2 * (n2 + 1) / 2+ 1

      hist(
        v$ranks,
        main = "Distribution of the W-statistic",
        xlab = "",
        ylab = "",
        yaxt = "n",
        freq = FALSE,
        xlim = c(xlim.min, xlim.max),
        cex.axis = 2,
        cex.main = 2
      )
      rug(jitter(v$ranks),
          lwd = 1,
          ticksize = 0.03)
      rug(v$ranks[[length(v$ranks)]],
          lwd = 10,
          ticksize = 0.05,
          col = "red")
      abline(v=v$W, col="blue", lwd=2)
    }
  })
}

shinyApp(ui = ui, server = server)
