plotlyContinuousUniformDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f16 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f16 <- dunif(xseq, as.numeric(input$UnifMin), as.numeric(input$UnifMax))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f16 <- punif(xseq, as.numeric(input$UnifMin), as.numeric(input$UnifMax))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f16, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        print(fig)
        xsize = length(xseq)
        newy = f16
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = punif(as.numeric(probrange[2]), as.numeric(input$UnifMin), as.numeric(input$UnifMax)) -
            punif(as.numeric(probrange[1]), as.numeric(input$UnifMin), as.numeric(input$UnifMax))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[16], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f16), max(f16))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}