comb <- function(n, k) {
    res <- seq_len(0)
    perm <- ifelse((k > n | k  < 0), 0, 
        for (x in k) {
            prod <- 1
            for (i in 1:x) {
                prod = prod * (n - i + 1)
            }
            append(res, prod)
        }
    )
    perm / factorial(k)
}

dWalkMax <- function(x, steps) {
    k <- round(x, 0)
    m <- ifelse( ((k + steps) %% 2 == 0), (k + steps)/2, (k + steps + 1)/2 )
    comb(steps, m) / (2 ^ steps)
}

pWalkMax <- function(x, steps) {
    0
}

plotlyWalkMaxDistribution <- function(plotrange, input, distType, probrange, session) {
    updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -pi,
        max = pi, step = NULL, timeFormat = NULL, timezone = NULL)
    xseq <- seq(min(-pi, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]),
        pi), 0.01)
    f72 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f72 <- dWalkMax(xseq, as.numeric(input$WalkMaxSteps))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f72 <- pWalkMax(xseq, as.numeric(input$WalkMaxSteps))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x = xseq, y = f72, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy")
        xsize <- length(xseq)
        newy <- f72
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob <- pWalkMax(as.numeric(probrange[2]), as.numeric(input$WalkMaxSteps)) - pWalkMax(as.numeric(probrange[1]), as.numeric(input$WalkMaxSteps))
        fig <- fig %>%
            add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        fig <- fig %>%
            plotly::layout(title = paste(distributions[15], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f72), max(f72))), xaxis = list(showticklabels = TRUE,
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
