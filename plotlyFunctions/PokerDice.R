dPokerDice <- function(x) {
    c <- 7776
    i <- round(x + 0.5, 0)
    ifelse(i == 0, 720.0 / c,
        ifelse(i == 1, 3600.0 / c,
            ifelse(i == 2, 1800.0 / c,
                ifelse(i == 3, 1200.0 / c,
                    ifelse(i == 4, 300.0 / c,
                        ifelse(i == 5, 150.0 / c,
                            ifelse(i == 6, 6.0 / c, 0)
                        )
                    )
                )
            )
        )
    )
}

getCDF_PD <- function(x) {
    c <- 7776
    i <- round(x + 0.5, 0)
    sum <- 0
    if (i < 0) {
        return(0)
    } else if (i > 6) {
        return(1)
    } else {
        for (index in 0:i) {
            if (index == 0) {
                sum <- sum + 720.0/c
            } else if (index == 1) {
                sum <- sum + 3600.0 / c
            } else if (index == 2) {
                sum <- sum + 1800.0 / c
            } else if (index == 3) {
                sum <- sum + 1200.0 / c
            } else if (index == 4) {
                sum <- sum + 300.0 / c
            } else if (index == 5) {
                sum <- sum + 150.0 / c
            } else if (index == 6) {
                sum <- sum + 6.0 / c
            }
        }
        return(sum)
    }
}

pPokerDice <- function(x) {
    sapply(x, getCDF_PD)
}

plotlyPokerDiceDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f62 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f62 <- dPokerDice(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f62 <- pPokerDice(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f62, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f62
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_PD(as.numeric(probrange[2])) - getCDF_PD(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[62], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f62), max(f62))
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
