dGilbrats <- function(x, mu, sigma) {
    C <- sqrt(2 * pi)
    z <- (log(x) - mu) / sigma
    exp(-z * z / 2) / (x * C * sigma)
}

pGilbrats <- function(x, mu, sigma) {
    z <- (log(x) - mu) / sigma
    ifelse(z >= 0, 0.5 + 0.5 * Rlab::pgamma(z * z / 2, 0.5), 0.5 - 0.5 * Rlab::pgamma(z * z / 2, 0.5))
}

plotlyGilbratsDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f31 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f31 <- dGilbrats(xseq, as.numeric(input$GilbratsMu), as.numeric(input$GilbratsSigma))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f31 <- pGilbrats(xseq, as.numeric(input$GilbratsMu), as.numeric(input$GilbratsSigma))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f31, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f31
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pGilbrats(as.numeric(probrange[2]), as.numeric(input$GilbratsMu), as.numeric(input$GilbratsSigma)) -
            pGilbrats(as.numeric(probrange[1]), as.numeric(input$GilbratsMu), as.numeric(input$GilbratsSigma)) 
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[31], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f31), max(f31))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
