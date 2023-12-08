comb_WalkMax <- function(n, k) {
    perm <- 0
    if (k > n | k < 0) { 
        perm <- 0
    } else {
        perm <- 1
        for (i in 1:k) {
            perm <- perm * (n - i + 1)
        }
    }
    perm / factorial(k)
}

getDensity_WalkMax <- function(x) {
    k <- round(x + 0.5, 0)

    if (k + steps_WalkMax <= 0) {
        return(0)
    }

    if (k > steps_WalkMax) {
        return(0)
    }

    m <- 0
    if ((k + steps_WalkMax) %% 2 == 0) {
        m <- (k + steps_WalkMax) / 2 
    } else {
        m <- (k + steps_WalkMax + 1) / 2
    }

    comb_WalkMax(steps_WalkMax, m) / (2 ^ steps_WalkMax)
}

getCDF_WalkMax <- function(x) {
    k <- round(x + 0.5, 0)

    if (k + steps_WalkMax <= 0) {
        return(0)
    }

    if (k > steps_WalkMax) {
        return(1)
    }

    sum <- 0

    for (i in (- steps_WalkMax):min(k, steps_WalkMax)) {
        sum <- sum + getDensity_WalkMax(i)
    }

    sum / 2
}

dWalkMax <- function(x) {
    sapply(x, getDensity_WalkMax)
}

pWalkMax <- function(x, steps) {
    sapply(x, getCDF_WalkMax)
}

plotlyWalkMaxDistribution <- function(plotrange, input, distType, probrange, session) {
    steps_WalkMax <<- as.numeric(input$WalkMaxSteps)

    # updateSliderInput(session, "plotrange",
    #     label = NULL, value = NULL, min = -pi,
    #     max = pi, step = NULL, timeFormat = NULL, timezone = NULL
    # )

    xseq <- seq(min(-pi, as.numeric(plotrange[1])), max(
        as.numeric(plotrange[2]),
        pi
    ), 0.01)

    f72 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f72 <- dWalkMax(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f72 <- pWalkMax(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f72, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f72
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_WalkMax(as.numeric(probrange[2])) - getCDF_WalkMax(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[72], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f72), max(f72))
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
