dError <- function(x, location, scale, shape) {
    NormalizingConst <- 1/(gamma(1 / shape))
    (NormalizingConst * shape / (2 * scale)) * exp(-( (abs(x - location) / scale) ^ shape ) )
}

pError <- function(x, location, scale, shape) {
    NormalizingConst <- 1/(gamma(1 / shape))

    0.5 + sign(x - location) * (0.5 * NormalizingConst) * pgamma((abs((x-location)/scale) ^ shape), (1/shape), 1, lower = TRUE)
}

plotlyErrorDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f22 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f22 <- dError(xseq, as.numeric(input$ErrorLocation), as.numeric(input$ErrorScale), as.numeric(input$ErrorShape))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f22 <- pError(xseq, as.numeric(input$ErrorLocation), as.numeric(input$ErrorScale), as.numeric(input$ErrorShape))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f22, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f22
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pError(as.numeric(probrange[2]), as.numeric(input$ErrorLocation), as.numeric(input$ErrorScale), as.numeric(input$ErrorShape)) -
            pError(as.numeric(probrange[1]), as.numeric(input$ErrorLocation), as.numeric(input$ErrorScale), as.numeric(input$ErrorShape))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[22], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f22), max(f22))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}



# NormalizingConst