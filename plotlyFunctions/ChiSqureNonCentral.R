plotlyChiSqureNonCentralDistribution <- function(plotrange, input, distType, probrange) {
    k = as.numeric(input$Chi2NCn)
    lemda = as.numeric(input$Chi2NCNCP)
    mean = k + lemda
    standard_dev = sqrt(2 * (k + 2 * lemda))
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
    f14 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f14 <- dchisq(xseq, as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f14 <- pchisq(xseq, as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f14, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        print(fig)
        xsize = length(xseq)
        newy = f14
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pchisq(as.numeric(probrange[2]), as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP)) -
            pchisq(as.numeric(probrange[1]), as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[14], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f14), max(f14))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
