dCircle <- function(x, radius) {
    ifelse((-radius <= x & x <= radius), (2 * sqrt(radius * radius - x * x) / (pi * radius * radius)), 0)
}

pCircle <- function(x, radius) {
    0.5 + asin(x / radius) / pi + x * sqrt(1 - x * x / (radius * radius)) / (pi * radius)
}

plotlyCircleDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f15 <- 0
    if (input$FunctionType == "PDF/PMF") {
        f15 <- dCircle(xseq, as.numeric(input$CircleRadius))
        graphtype <- "PMF"
    } else if (input$FunctionType == "CDF/CMF") {
        f15 <- pCircle(xseq, as.numeric(input$CircleRadius))
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

        prob <- pCircle(as.numeric(probrange[2]), as.numeric(input$CircleRadius)) - pCircle(as.numeric(probrange[1]), as.numeric(input$CircleRadius))

        fig <- plot_ly(
            x = xseq, y = f15, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )

        fig <- fig %>%
            add_trace(
                x = xseq, y = f15, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[15], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f15), max(f15)), type = "linear"
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
