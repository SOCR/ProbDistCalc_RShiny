plotlyVonMisesDistribution <- function(plotrange, input, distType, probrange, session) {
    updateSliderInput(session, "plotrange",
        label = NULL, value = NULL, min = -pi,
        max = pi, step = NULL, timeFormat = NULL, timezone = NULL
    )
    xseq <- seq(min(-pi, as.numeric(plotrange[1])), max(
        as.numeric(plotrange[2]),
        pi
    ), 0.01)
    f71 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f71 <- circular::dvonmises(xseq, as.numeric(input$vonMisesMu), as.numeric(input$vonMisesKappa))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        cat("vonMisesMu data type: ", class(input$vonMisesMu), "\n")
        cat("vonMisesKappa data type: ", class(input$vonMisesKappa), "\n")
        f71 <- circular::pvonmises(xseq, as.numeric(input$vonMisesMu), as.numeric(input$vonMisesKappa))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f71, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f71
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- circular::pvonmises(
            as.numeric(probrange[2]), as.numeric(input$vonMisesMu),
            as.numeric(input$vonMisesKappa)
        ) - circular::pvonmises(
            as.numeric(probrange[1]),
            as.numeric(input$vonMisesMu), as.numeric(input$vonMisesKappa)
        )
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[15], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f71), max(f71))
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
