plotlyGompertzDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f32 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f32 <- dgompertz(xseq, as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f32 <- pgompertz(xseq, as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f32, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        print(fig)
        xsize = length(xseq)
        newy = f32
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pgompertz(as.numeric(plotrange[2]), as.numeric(input$Gompertz_N),
            as.numeric(input$Gompertz_B)) - pgompertz(as.numeric(plotrange[1]), as.numeric(input$Gompertz_N),
            as.numeric(input$Gompertz_B))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[33], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f32), max(f32))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
