dTwoSidedPower <- function(x, left, right, med, power) {
    c <- power/(right - left)
    ifelse (((x <= left) | (x >= right)), 0, 
        ifelse( ((x > left) & (x < med)), c * ( ((x-left)/(med-left)) ^ (power - 1) ), c * ( ( (right - x)/(right - med) ) ^ (power - 1) ) )
    )
}

pTwoSidedPower <- function(x, left, right, med, power) {
    0
}

plotlyTwoSidedPowerDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f69 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f69 <- dTwoSidedPower(xseq, as.numeric(input$TSPowerLeft), as.numeric(input$TSPowerRight)
        , as.numeric(input$TSPowerMed), as.numeric(input$TSPowerPower))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f69 <- pTwoSidedPower(xseq, as.numeric(input$TSPowerLeft), as.numeric(input$TSPowerRight)
        , as.numeric(input$TSPowerMed), as.numeric(input$TSPowerPower))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f69, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f69
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pTwoSidedPower(as.numeric(probrange[2]), as.numeric(input$TSPowerLeft), as.numeric(input$TSPowerRight)
        , as.numeric(input$TSPowerMed), as.numeric(input$TSPowerPower)) - pTwoSidedPower(as.numeric(probrange[1]), as.numeric(input$TSPowerLeft), as.numeric(input$TSPowerRight)
        , as.numeric(input$TSPowerMed), as.numeric(input$TSPowerPower)) 
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[69], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f69), max(f69))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
