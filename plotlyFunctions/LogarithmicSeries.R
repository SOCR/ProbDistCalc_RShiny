zero <- function(x) {
    if (x <= 0) {
        0.0001
    } else {
        x
    }
}

plotlyLogarithmicSeriesDistribution <- function(
    plotrange, input, distType, probrange,
    session) {
    p <- as.numeric(input$LogP)
    mean <- 1 / -(log(1 - p, exp(1))) * p / (1 - p)
    standard_dev <- sqrt(mean / (1 - p) * (1 - p / -(log(1 - p, exp(1)))))
    if (plotrange[2] != mean + as.numeric(input$SDNum) * standard_dev && input$numericalValues ==
        0) {
        if (input$SDNum > 0) {
            updateSliderInput(session, "plotrange", label = NULL, value = c(0, mean +
                as.numeric(input$SDNum) * standard_dev), min = 1, max = mean + as.numeric(input$SDNum) *
                standard_dev, step = NULL, timeFormat = NULL, timezone = NULL)
            old_SD <- input$SDNum
            plotrange[2] <- mean + as.numeric(input$SDNum) * standard_dev
            plotrange[1] <- 1
        } else {
            updateSliderInput(session, "plotrange",
                label = NULL, value = NULL, min = -1000,
                max = 1000, step = NULL, timeFormat = NULL, timezone = NULL
            )
            old_SD <- input$SDNum
        }
    }
    if (old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = c(0, mean +
            as.numeric(input$SDNum) * standard_dev), min = 1, max = mean + as.numeric(input$SDNum) *
            standard_dev, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD <- input$SDNum
        plotrange[2] <- mean + as.numeric(input$SDNum) * standard_dev
        plotrange[1] <- 1
    } else if (input$numericalValues == 0 && input$SDNum <= 0) {
        updateSliderInput(session, "plotrange",
            label = NULL, value = NULL, min = -1000,
            max = 1000, step = NULL, timeFormat = NULL, timezone = NULL
        )
        old_SD <- input$SDNum
    }
    xseq <- seq(round(max(1, as.numeric(plotrange[1])), 0), round(max(
        as.numeric(plotrange[2]),
        10
    ), 0), 1)
    f43 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f43 <- dlogseries(xseq, as.numeric(input$LogP))
        graphtype <- "PMF"
    } else if (input$FunctionType == "CDF/CMF") {
        f43 <- plogseries(xseq, as.numeric(input$LogP))
        graphtype <- "CMF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        xsize <- length(xseq)
        colors <- c(rep("rgb(31, 119, 180)", xsize))
        for (index in 1:xsize) {
            if (xseq[index] >= round(probrange[1], 0) && xseq[index] <= round(
                probrange[2],
                0
            )) {
                colors[index] <- "rgb(255, 127, 14)"
            }
        }
        fig <- plot_ly(
            x = xseq, y = f43, name = distType, type = "bar", marker = list(color = colors),
            text = f43, hovertemplate = paste(
                "<br><b>Prob. </b>: %{y}</br>", "<b>X</b>: %{x}",
                "<b>Y</b>: %{y}"
            ),
        )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[43], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f43), max(f43)), type = "linear"
                ),
                xaxis = list(
                    showticklabels = TRUE, title = "* All x values rounded to nearest integers",
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
