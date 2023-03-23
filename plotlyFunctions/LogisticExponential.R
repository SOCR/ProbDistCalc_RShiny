plotlyLogisticExponentialDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f45 <- vector()
    graphtype <- ""
    Beta = as.numeric(input$LogEx_B)
    Alpha = as.numeric(input$LogEx_A)
    if (input$FunctionType == "PDF/PMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] > 0) {
                f45 <- append(f45, (Alpha * Beta * ((exp(Alpha * xseq[index]) - 1)^(Beta -
                  1)) * exp(Alpha * xseq[index]))/((1 + (exp(Alpha * xseq[index]) -
                  1)^Beta)^2))
            } else {
                f45 <- append(f45, 0)
            }
        }
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        for (index in 1:length(xseq)) {
            if (xseq[index] > 0) {
                # (exp(Alpha * xseq[index] ) - 1) ** Beta)/(1 + ( exp(Alpha *
                # (xseq[index] ) - 1) ** Beta))
                f45 <- append(f45, (exp(Alpha * xseq[index]) - 1)^Beta/(1 + (exp(Alpha *
                  xseq[index]) - 1)^Beta))
            } else {
                f45 <- append(f45, 0)
            }
        }
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f45, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f45
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = 0

        if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
            prob = 0
        } else if (as.numeric(probrange[1]) <= 0) {
            prob = ((exp(Alpha * (as.numeric(probrange[2]))) - 1)^Beta)/(1 + (exp(Alpha *
                (as.numeric(probrange[2]))) - 1)^Beta)
        } else if (as.numeric(probrange[1]) > 0 && as.numeric(probrange[2]) > 0) {
            prob = ((exp(Alpha * (as.numeric(probrange[2]))) - 1)^Beta)/(1 + (exp(Alpha *
                (as.numeric(probrange[2]))) - 1)^Beta)
            -((exp(Alpha * (as.numeric(probrange[1]))) - 1)^Beta)/(1 + (exp(Alpha *
                (as.numeric(probrange[1]))) - 1)^Beta)
        }
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[45], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f45), max(f45))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
