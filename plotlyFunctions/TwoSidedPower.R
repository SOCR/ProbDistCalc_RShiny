getDensity_TSP <- function(x) {
    c <- power_TSP / (r_TSP - l_TSP)

    if (x <= l_TSP || x >= r_TSP) {
        return(0)
    } else if (x > l_TSP && x <= med_TSP) {
        return(c * (((x - l_TSP) / (med_TSP - l_TSP))^(power_TSP - 1)))
    } else {
        return(c * (((r_TSP - x) / (r_TSP - med_TSP))^(power_TSP - 1)))
    }
}

dTwoSidedPower <- function(x) {
    sapply(x, getDensity_TSP)
}

getCDF_TSP <- function(x) {
    # TODO: CDF
}

pTwoSidedPower <- function(x) {
    sapply(x,getCDF_TSP)
}

plotlyTwoSidedPowerDistribution <- function(plotrange, input, distType, probrange) {
    l_TSP <<- as.numeric(input$TSPowerLeft)
    r_TSP <<- as.numeric(input$TSPowerRight)
    med_TSP <<- as.numeric(input$TSPowerMed)
    power_TSP <<- as.numeric(input$TSPowerPower)
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f69 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f69 <- dTwoSidedPower(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f69 <- pTwoSidedPower(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f69, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f69
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_TSP(as.numeric(probrange[2])) - getCDF_TSP(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[69], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f69), max(f69))
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
