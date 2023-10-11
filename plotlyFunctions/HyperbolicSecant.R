#------------------------------------------------------------------
# PDF
#------------------------------------------------------------------

dsech <- Vectorize(function(x, mu, sigma, log = FALSE) {
    logden <- -log(2) - log(sigma) - log(cosh(0.5 * pi * (x - mu)/sigma))
    val <- ifelse(log, logden, exp(logden))
    return(val)
})

#------------------------------------------------------------------
# CDF
#------------------------------------------------------------------

psech <- Vectorize(function(x, mu, sigma, log.p = FALSE) {
    logcdf <- log(2) - log(pi) + log(atan(exp(0.5 * pi * (x - mu)/sigma)))
    val <- ifelse(log.p, logcdf, exp(logcdf))
    return(val)
})

plotlyHyperbolicSecantDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f36 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f36 <- dsech(xseq, as.numeric(input$HSmu), as.numeric(input$HSsigma))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f36 <- psech(xseq, as.numeric(input$HSmu), as.numeric(input$HSsigma))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f36, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f36
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = psech(as.numeric(probrange[2]), as.numeric(input$HSmu), as.numeric(input$HSsigma)) -
            psech(as.numeric(probrange[1]), as.numeric(input$HSmu), as.numeric(input$HSsigma))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[36], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f36), max(f36))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
