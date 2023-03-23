plotlyLogNormalDistribution <- function(plotrange, input, distType, probrange) {
    u = as.numeric(input$LogNormMean)
    sig = as.numeric(input$LogNormSD)
    mean = exp((u + (sig^2/2)))
    standard_dev = sqrt((exp(sig^2) - 1) * exp(2 * u + sig^2))
    if (plotrange[2] != mean + as.numeric(input$SDNum) * standard_dev && input$numericalValues ==
        0) {
        if (input$SDNum > 0) {
            updateSliderInput(session, "plotrange", label = NULL, value = c(0, mean +
                as.numeric(input$SDNum) * standard_dev), min = 0, max = mean + as.numeric(input$SDNum) *
                standard_dev, step = NULL, timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum) * standard_dev
            plotrange[1] = 0
        } else {
            updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
                max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
        }
    }
    if (old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = c(0, mean +
            as.numeric(input$SDNum) * standard_dev), min = 0, max = mean + as.numeric(input$SDNum) *
            standard_dev, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD = input$SDNum
        plotrange[2] = mean + as.numeric(input$SDNum) * standard_dev
        plotrange[1] = 0
    } else if (input$numericalValues == 0 && input$SDNum <= 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
            max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD = input$SDNum
    }
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f46 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f46 <- dlnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f46 <- plnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f46, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize = length(xseq)
        newy = f46
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = plnorm(as.numeric(probrange[2]), as.numeric(input$LogNormMean), as.numeric(input$LogNormSD)) -
            plnorm(as.numeric(probrange[1]), as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[46], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f46), max(f46))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
