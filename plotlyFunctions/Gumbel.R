plotlyGumbelDistribution <- function(plotrange, input, distType, probrange, session) {
    Beta = as.numeric(input$Gumbel_Beta)
    U = as.numeric(input$Gumbel_U)
    mean = U + Beta * 0.57721
    standard_dev = sqrt(pi^2/6 * Beta^2)
    if (plotrange[2] != mean + as.numeric(input$SDNum) * standard_dev && input$numericalValues ==
        0) {
        if (input$SDNum > 0) {
            updateSliderInput(session, "plotrange", label = NULL, value = c(mean -
                as.numeric(input$SDNum) * standard_dev, mean + as.numeric(input$SDNum) *
                standard_dev), min = mean - as.numeric(input$SDNum) * standard_dev,
                max = mean + as.numeric(input$SDNum) * standard_dev, step = NULL,
                timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum) * standard_dev
            plotrange[1] = mean - as.numeric(input$SDNum) * standard_dev
        } else {
            updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
                max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
        }
    }
    if (old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = c(mean - as.numeric(input$SDNum) *
            standard_dev, mean + as.numeric(input$SDNum) * standard_dev), min = mean -
            as.numeric(input$SDNum) * standard_dev, max = mean + as.numeric(input$SDNum) *
            standard_dev, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD = input$SDNum
        plotrange[2] = mean + as.numeric(input$SDNum) * standard_dev
        plotrange[1] = mean - as.numeric(input$SDNum) * standard_dev
    } else if (input$numericalValues == 0 && input$SDNum <= 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
            max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD = input$SDNum
    }
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f33 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f33 <- dgumbel(xseq, as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f33 <- pgumbel(xseq, as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f33, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        print(fig)
        xsize = length(xseq)
        newy = f33
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = pgumbel(as.numeric(probrange[2]), as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta)) -
            pgumbel(as.numeric(probrange[1]), as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[33], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f33), max(f33))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
