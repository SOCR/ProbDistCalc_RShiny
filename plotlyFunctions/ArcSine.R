plotlyArcSineDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f2 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f2 <- darcsine(xseq, as.numeric(input$ArcSineA), as.numeric(input$ArcSineB))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f2 <- parcsine(xseq, as.numeric(input$ArcSineA), as.numeric(input$ArcSineB))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f2, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f2
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- parcsine(as.numeric(probrange[2]), as.numeric(input$ArcSineA), as.numeric(input$ArcSineB)) -
            parcsine(as.numeric(probrange[1]), as.numeric(input$ArcSineA), as.numeric(input$ArcSineB))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[2], " - ", graphtype, sep = ""),
                autosize = TRUE, hovermode = "x", hoverlabel = list(namelength = 100),
                yaxis = list(fixedrange = TRUE, zeroline = TRUE, range = c(
                    min(f2),
                    max(f2)
                )), xaxis = list(
                    showticklabels = TRUE, zeroline = TRUE,
                    showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
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
