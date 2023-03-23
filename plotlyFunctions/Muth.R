plotlyMuthDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f53 <- vector()
    graphtype <- ""
    K = as.numeric(input$MuthKappa)
    if (input$FunctionType == "PDF/PMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] > 0) {
                f53 <- append(f53, (exp(K * xseq[index]) - K) * exp(-(exp(K * xseq[index]))/K +
                  K * xseq[index] + 1/K))
            } else {
                f53 <- append(f53, 0)
            }
        }
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] > 0) {
                f53 <- append(f53, 1 - exp(-(exp(K * xseq[index]))/K + K * xseq[index] +
                  1/K))
            } else {
                f53 <- append(f53, 0)
            }
        }
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f53, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f53
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = 0
        if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
            prob = 0
        } else if (as.numeric(probrange[2]) <= 0) {
            prob = 0
        } else if (as.numeric(probrange[1]) <= 0) {
            prob = 1 - exp(-(exp(K * as.numeric(probrange[2])))/K + K * as.numeric(probrange[2]) +
                1/K)
        } else {
            prob = (1 - exp(-(exp(K * as.numeric(probrange[2])))/K + K * as.numeric(probrange[2]) +
                1/K)) - (1 - exp(-(exp(K * as.numeric(probrange[1])))/K + K * as.numeric(probrange[1]) +
                1/K))
        }
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[53], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f53), max(f53))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
