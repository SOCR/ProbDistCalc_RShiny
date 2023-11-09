plotlyJohnsonSBBoundedDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f39 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f39 <- dJohnsonSB(
            xseq, as.numeric(input$JohnSBgamma), as.numeric(input$JohnSBdelta),
            as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda)
        )
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f39 <- pJohnsonSB(
            xseq, as.numeric(input$JohnSBgamma), as.numeric(input$JohnSBdelta),
            as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda)
        )
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f39, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f39
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- pJohnsonSB(
            as.numeric(probrange[2]), as.numeric(input$JohnSBgamma),
            as.numeric(input$JohnSBdelta), as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda)
        ) -
            pJohnsonSB(
                as.numeric(probrange[1]), as.numeric(input$JohnSBgamma), as.numeric(input$JohnSBdelta),
                as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda)
            )
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[39], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f39), max(f39))
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
