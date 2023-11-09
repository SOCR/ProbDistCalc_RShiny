plotlyUQuadraticDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f70 <- vector()
    graphtype <- ""
    W <- as.numeric(input$UQ_W)
    C <- as.numeric(input$UQ_C)
    if (input$FunctionType == "PDF/PMF") {
        for (index in 1:length(xseq)) {
            if (C - W <= xseq[index] && xseq[index] <= W + C) {
                f70 <- append(f70, 3 / (2 * W) * ((xseq[index] - C) / W)^2)
            } else {
                f70 <- append(f70, 0)
            }
        }
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        for (index in 1:length(xseq)) {
            if (C - W <= xseq[index] && xseq[index] <= W + C) {
                f70 <- append(f70, 0.5 * (1 + ((xseq[index] - C) / W)^3))
            } else {
                f70 <- append(f70, 0)
            }
        }
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f70, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f70
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- 0
        if (as.numeric(probrange[2]) >= W + C && as.numeric(probrange[1] <= C - W)) {
            prob <- 1
        } else if (as.numeric(probrange[2]) <= C - W && as.numeric(probrange[1] <=
            C - W)) {
            prob <- 0
        } else if (as.numeric(probrange[2]) >= W + C) {
            prob <- 1 - 0.5 * (1 + ((as.numeric(probrange[1]) - C) / W)^3)
        } else if (as.numeric(probrange[1] <= C - W)) {
            prob <- 0.5 * (1 + ((as.numeric(probrange[2]) - C) / W)^3)
        } else {
            prob <- 0.5 * (1 + ((as.numeric(probrange[2]) - C) / W)^3) - 0.5 * (1 +
                ((as.numeric(probrange[1]) - C) / W)^3)
        }
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[70], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f70), max(f70))
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
