getDensity_Match <- function(x) {
    k <- round(x - 0.5, 0)
    sum <- 0
    sign <- -1
    if (param_Match - k > 0) {
        for (j in 0:(param_Match - k)) {
            sign <- -sign
            sum <- sum + sign / factorial(j)
        }
        sum / factorial(k)
    } else {
        0
    }

}

dMatch <- function(x) {
    sapply(x, getDensity_Match)
}

getCDF_Match <- function(x) {
    k <- round(x - 0.5, 0)

    sum <- 0

    for (i in 0:k) {
        sum <- sum + getDensity_Match(i + 0.5)
    }

    sum
}

pMatch <- function(x) {
    sapply(x, getCDF_Match)
}

plotlyMatchingDistribution <- function(plotrange, input, distType, probrange) {
    param_Match <<- as.numeric(input$MatchParam)
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f48 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f48 <- dMatch(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f48 <- pMatch(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f48, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f48
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- pMatch(as.numeric(probrange[2])) - pMatch(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[48], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f48), max(f48))
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
