plotlyNormalDistribution <- function(plotrange, input, distType, probrange, session, fig) {
  print(dataset)
  old_SD <- 0
  if (plotrange[2] != as.numeric(input$NormMean) + as.numeric(input$SDNum) * as.numeric(input$NormSD) &&
      input$numericalValues == 0) {
    if (input$SDNum > 0) {
      updateSliderInput(session, "plotrange",
                        label = NULL, value = c(as.numeric(input$NormMean) -
                                                  as.numeric(input$SDNum) * as.numeric(input$NormSD), as.numeric(input$NormMean) +
                                                  as.numeric(input$SDNum) * as.numeric(input$NormSD)), min = as.numeric(input$NormMean) -
                          as.numeric(input$SDNum) * as.numeric(input$NormSD), max = as.numeric(input$NormMean) +
                          as.numeric(input$SDNum) * as.numeric(input$NormSD), step = NULL,
                        timeFormat = NULL, timezone = NULL
      )
      old_SD <- input$SDNum
      plotrange[2] <- as.numeric(input$NormMean) + as.numeric(input$SDNum) *
        as.numeric(input$NormSD)
      plotrange[1] <- as.numeric(input$NormMean) - as.numeric(input$SDNum) *
        as.numeric(input$NormSD)
    } else {
      updateSliderInput(session, "plotrange",
                        label = NULL, value = NULL, min = -1000,
                        max = 1000, step = NULL, timeFormat = NULL, timezone = NULL
      )
      old_SD <- input$SDNum
    }
  }
  if (old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0) {
    updateSliderInput(session, "plotrange",
                      label = NULL, value = c(as.numeric(input$NormMean) -
                                                as.numeric(input$SDNum) * as.numeric(input$NormSD), as.numeric(input$NormMean) +
                                                as.numeric(input$SDNum) * as.numeric(input$NormSD)), min = as.numeric(input$NormMean) -
                        as.numeric(input$SDNum) * as.numeric(input$NormSD), max = as.numeric(input$NormMean) +
                        as.numeric(input$SDNum) * as.numeric(input$NormSD), step = NULL, timeFormat = NULL,
                      timezone = NULL
    )
    old_SD <- input$SDNum
    plotrange[2] <- as.numeric(input$NormMean) + as.numeric(input$SDNum) * as.numeric(input$NormSD)
    plotrange[1] <- as.numeric(input$NormMean) - as.numeric(input$SDNum) * as.numeric(input$NormSD)
  } else if (input$numericalValues == 0 && input$SDNum <= 0) {
    updateSliderInput(session, "plotrange",
                      label = NULL, value = NULL, min = -1000,
                      max = 1000, step = NULL, timeFormat = NULL, timezone = NULL
    )
    old_SD <- input$SDNum
  }
  
  # fitNormalModel <- fitNormal(dataset[,input$outcome])
  xseq <- seq( min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2])),0.01)
  x <- rnorm(10000, as.numeric(input$NormMean), as.numeric(input$NormSD))
  f57 <- 0
  graphtype <- ""
  if (input$FunctionType == "PDF/PMF") {
    f57 <- dnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
    graphtype <- "PDF"
  } else if (input$FunctionType == "CDF/CMF") {
    f57 <- pnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
    graphtype <- "CDF"
  } else {
    graphtype <- ""
  }
  if (graphtype != "") {
    # fig <- plot_ly(data = dataset, x = dataset$Sepal.Length, type = "histogram",
    #                histnorm = "probability",
    #                name = "Histogram Layer",
    #                size = 0.4)
    fig <- plot_ly(data = dataset, x = ~dataset[, input$outcome], type = "histogram",
                   histnorm = "probability",
                   name = "Histogram Layer",
                   size = 0.4)
    fig <- fig %>% add_lines(
      x = xseq, y = f57, name = distType, type = "scatter", mode = "lines", line = list(width = 2)
    )
    xsize <- length(xseq)
    newy <- f57
    for (index in 1:xsize) {
      if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
        newy[index] <- NA
      }
    }
    prob <- pnorm(as.numeric(probrange[2]), as.numeric(input$NormMean), as.numeric(input$NormSD)) -
      pnorm(as.numeric(probrange[1]), as.numeric(input$NormMean), as.numeric(input$NormSD))
    fig <- fig %>%
      add_lines(
        x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
        hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)", line = list(width = 2)
      )
    fig <- fig %>%
      plotly::layout(
        title = paste(distributions[57], " - ", graphtype, sep = ""),
        hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
          fixedrange = TRUE,
          zeroline = TRUE, range = c(min(f57), max(f57))
        ), xaxis = list(
          showticklabels = TRUE,
          zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
          linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(
            plotrange[1],
            plotrange[2]
          )
        ), showlegend = FALSE
      )


    fig <- fig %>%
      config(editable = FALSE)
    
    fig
  }
}