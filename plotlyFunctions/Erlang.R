dErlang <- function(x, scale, shape) {
    NormalizingConst = scale * factorial(shape-1)
    ifelse( (x < 0), 0, ((exp(-x/scale) * ( (x/scale) ^ (shape-1) ))/NormalizingConst))
}

pErlang <- function(x, scale, shape) {
    res <- numeric(length(x))
    NormalizingConst = scale * factorial(shape-1)
    
    for (i in 0 : (shape-1)) {
        res = res + ((x/scale) ^ i) / factorial(i)
    }

    (1- exp(-x/scale) * res)
}

plotlyErlangDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f21 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f21 <- dErlang(xseq, as.numeric(input$ErlangScale), as.numeric(input$ErlangShape))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f21 <- pErlang(xseq, as.numeric(input$ErlangScale), as.numeric(input$ErlangShape))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f21, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f21
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pErlang(as.numeric(probrange[2]), as.numeric(input$ErlangScale), as.numeric(input$ErlangShape)) -
            pErlang(as.numeric(probrange[1]), as.numeric(input$ErlangScale), as.numeric(input$ErlangShape))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[21], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f21), max(f21))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}



# NormalizingConst