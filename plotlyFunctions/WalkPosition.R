comb_WalkPos <- function(n, k) {
    perm <- 0
    if (k > n || k < 0) {
        perm <- 0
    } else {
        if (k == 0) {
            perm <- 1
        } else {
            prod <- 1
            for (i in 1:k) {
                prod <- prod * (n - i + 1)
            }
            perm <- prod
        }
    }
    perm / factorial(k)
}

getDensity_WalkPos <- function(x) {
    k <- round(x + 0.5)
    m <- (k + steps_WalkPos) / 2

    return(comb_WalkPos(steps_WalkPos, m) * ( prob_WalkPos ^ m ) * ( (1 - prob_WalkPos) ^ (steps_WalkPos - m) ))
}

getCDF_WalkPos <- function(x) {
    k <- round(x + 0.5)

    if (k < -steps_WalkPos) {
        return(0)
    }

    if (k > steps_WalkPos) {
       k <- steps_WalkPos
    }

    sum <- 0

    for (i in -steps_WalkPos:k) {
        m <- (i + steps_WalkPos) / 2
        sum <- sum + comb_WalkPos(steps_WalkPos, m) * ( prob_WalkPos ^ m ) * ( (1 - prob_WalkPos) ^ (steps_WalkPos - m) )
    }

    return(sum)
}

dWalkPos <- function(x) {
    sapply(x, getDensity_WalkPos)
}

pWalkPos <- function(x) {
    sapply(x, getCDF_WalkPos)
}

plotlyWalkPositionDistribution <- function(plotrange, input, distType, probrange) {
    steps_WalkPos <<- as.numeric(input$WalkPosSteps)
    prob_WalkPos <<- as.numeric(input$WalkPosP)
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f73 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f73 <- dWalkPos(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f73 <- pWalkPos(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f73, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f73
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_WalkPos(as.numeric(probrange[2])) - getCDF_WalkPos(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distType, " - ", graphtype, sep = ""), hovermode = "x",
                hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f73), max(f73))
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