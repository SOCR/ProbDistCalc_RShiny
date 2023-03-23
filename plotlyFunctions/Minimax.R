plotlyMinimaxDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f50 <- vector()
    graphtype <- ""
    Beta = as.numeric(input$Mini_B)
    Gamma = as.numeric(input$Mini_V)
    if (input$FunctionType == "PDF/PMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] < 1 && xseq[index] > 0) {
                f50 <- append(f50, Beta * Gamma * (xseq[index])^(Beta - 1) * (1 -
                  (xseq[index])^Beta)^(Gamma - 1))
            } else {
                f50 <- append(f50, 0)
            }
        }
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] < 1 && xseq[index] > 0) {
                f50 <- append(f50, 1 - (1 - (xseq[index])^Beta)^(Gamma))
            } else {
                f50 <- append(f50, 0)
            }
        }
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f50, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f50
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = 0
        if (as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) <= 0) {
            prob = 1
        } else if (as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) >= 1) {
            prob = 0
        } else if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
            prob = 0
        } else if (as.numeric(probrange[2]) >= 1) {
            prob = 1 - (1 - (1 - (as.numeric(probrange[1]))^Beta)^(Gamma))
        } else if (as.numeric(probrange[1]) <= 0) {
            prob = 1 - (1 - (as.numeric(probrange[2]))^Beta)^(Gamma)
        } else {
            prob = 1 - (1 - (as.numeric(probrange[2]))^Beta)^(Gamma) - (1 - (1 -
                (as.numeric(probrange[1]))^Beta)^(Gamma))
        }
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[50], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f50), max(f50))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
