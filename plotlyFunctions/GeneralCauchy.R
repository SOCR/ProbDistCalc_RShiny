dGeneralCauchy <- function(x, alpha, beta) {
    coef <- 1.0 / (pi * beta)
    temp <- (x - alpha) / beta
    coef / (1.0 + temp * temp)
}

pGeneralCauchy <- function(x, alpha, beta) {
    (0.5 + atan2(x - alpha, beta) / pi)
}

plotlyGeneralCauchyDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f28 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f28 <- dGeneralCauchy(xseq, as.numeric(input$GeneralCauchyAlpha), as.numeric(input$GeneralCauchyBeta))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f28 <- pGeneralCauchy(xseq, as.numeric(input$GeneralCauchyAlpha), as.numeric(input$GeneralCauchyBeta))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f28, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f28
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- pGeneralCauchy(as.numeric(probrange[2]), as.numeric(input$GeneralCauchyAlpha), as.numeric(input$GeneralCauchyBeta)) -
            pGeneralCauchy(as.numeric(probrange[1]), as.numeric(input$GeneralCauchyAlpha), as.numeric(input$GeneralCauchyBeta))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[28], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f28), max(f28))
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
