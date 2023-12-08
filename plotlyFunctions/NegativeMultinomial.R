correct_negMult <- function() {
    sumPi <- 0.0

    if (gamma_negMult <= 0) {
        gamma_negMult <<- 1
    }

    if (length(prob_negMult) < 1) {
        prob_negMult <<- c(0.1)
        x_negMult <<- c(1)
    }

    dimension <- length(prob_negMult)

    for (i in 1:dimension) {
        sumPi <- sumPi + prob_negMult[i]
    }

    if (sumPi <= 0.0 || sumPi >= 1.0) {
        prob_negMult <<- c(0.1)
        x_negMult <<- c(1)
    }
}

getProb <- function() {
    dimension <- length(prob_negMult)
    sumPi <- 0
    sumXi <- 0
    sumLnXiFact <- 0
    sumXiLnPi <- 0

    for (i in 1:dimension) {
        sumPi <- sumPi + prob_negMult[i]
        sumXi <- sumXi + x_negMult[i]
        sumLnXiFact <- sumLnXiFact + lfactorial(x_negMult[i])
        sumXiLnPi <- sumXiLnPi + x_negMult[i] * log(prob_negMult[i])
    }

    p0 <- 1.0 - sumPi

    exp(lgamma(gamma_negMult + sumXi) - (lgamma(gamma_negMult) + sumLnXiFact) + gamma_negMult * log(p0) + sumXiLnPi)
}

getDensity_negMult <- function(x) {
    getProb()
}

dNegMult <- function(x) {
    sapply(x, getDensity_negMult)
}

getCDF <- function(x) {
    0
}

pNegMult <- function(x) {
    sapply(x, getCDF)
}

plotlyNegativeMultinomialDistribution <- function(plotrange, input, distType, probrange){
    prob_negMult <<- as.numeric(unlist(strsplit(input$NegMultP,",")))
    x_negMult <<- as.numeric(unlist(strsplit(input$NegMultX,",")))
    gamma_negMult <<- as.numeric(input$NegMultGamma)

    correct_negMult()

    xseq <- seq(round(min(0, as.numeric(plotrange[1])), 0), round(max(
        as.numeric(plotrange[2]),
        10
    ), 0), 1)

    f56 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f56 <- dNegMult(xseq)
        graphtype <- "PMF"
    } else if (input$FunctionType == "CDF/CMF") {
        f56 <- pNegMult(xseq)
        graphtype <- "CMF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        xsize <- length(xseq)
        colors <- c(rep("rgb(31, 119, 180)", xsize))
        for (index in 1:xsize) {
            if (xseq[index] >= round(probrange[1], 0) && xseq[index] <= round(
                probrange[2],
                0
            )) {
                colors[index] <- "rgb(255, 127, 14)"
            }
        }
        fig <- plot_ly(
            x = xseq, y = f56, name = distType, type = "bar", marker = list(color = colors),
            text = f56, hovertemplate = paste(
                "<br><b>Prob. </b>: %{y}</br>", "<b>X</b>: %{x}",
                "<b>Y</b>: %{y}"
            ),
        )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[54], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f56), max(f56)), type = "linear"
                ),
                xaxis = list(
                    showticklabels = TRUE, title = "* All x values rounded to nearest integers",
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
