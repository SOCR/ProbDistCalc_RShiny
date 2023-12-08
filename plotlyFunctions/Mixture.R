getDensity_Mix <- function(x) {
    C <- sqrt(2 * pi)
    d <- 0
    z1 <- (x - mu1_Mix) / sigma1_Mix
    d1 <- exp(-z1*z1 / 2) / (C * sigma1_Mix)
    z2 <- (x - mu2_Mix) / sigma2_Mix
    d2 <- exp(-z2*z2 / 2) / (C * sigma2_Mix)
    
    d <- d + prob1_Mix * d1 + prob2_Mix * d2

    return(d)
}

getCDF_Mix <- function(x) {
    d <- prob1_Mix * pnorm(x, mean = mu1_Mix, sd = sigma1_Mix) + prob2_Mix * pnorm(x, mean = mu2_Mix, sd = sigma2_Mix)
    return(d)
}

dMix <- function(x) {
    sapply(x, getDensity_Mix)
}

pMix <- function(x) {
    sapply(x, getCDF_Mix)
}

plotlyMixtureDistribution <- function(plotrange, input, distType, probrange) {
    mu1_Mix <<- as.numeric(input$MixMu1)
    sigma1_Mix <<- as.numeric(input$MixSigma1)
    prob1_Mix <<- as.numeric(input$MixProb1)
    mu2_Mix <<- as.numeric(input$MixMu2)
    sigma2_Mix <<- as.numeric(input$MixSigma2)
    prob2_Mix <<- as.numeric(input$MixProb2)

    if (prob1_Mix + prob2_Mix != 1) {
        prob2_Mix <<- 1 - prob1_Mix
    }

    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f51 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f51 <- dMix(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f51 <- pMix(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f51, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f51
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Mix(as.numeric(probrange[2])) - getCDF_Mix(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f51), max(f51))
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