popSize <- 0
sampleSize <- 0

prob_birt <- array(0, c(0, 0))

calculate <- function() {
    temp <- rep(0, (popSize+1)*(sampleSize+1))

    prob_birt <<- array(temp, dim=c(popSize+1, sampleSize+1))
    prob_birt[1,1] <<- 1
    prob_birt[2,2] <<- 1

    for (i in 2:sampleSize) {
        if (i < popSize + 1) {
            upperIndex <- i + 1
        } else {
            upperIndex <- popSize + 1
        }
        for (j in 2:upperIndex) {
            prob_birt[j,i+1] <<- prob_birt[j, i] * (j/popSize) + prob_birt[j-1, i] * ( (popSize - j + 1) / popSize )
        }
    }
}

getDensity_Birt <- function(x) {
    x = round(x)
    if (x<=0) {
        return(0)
    }
    return(prob_birt[x, sampleSize+1])
}

getCDF_Birt <- function(x) {
    x = round(x)
    if (x<=0) {
        return(0)
    }

    res <- 0

    for (i in 1:x) {
        res = res + prob_birt[i, sampleSize+1]
    }
    return(res)
}

dBirt <- function(x) {
    calculate()
    sapply(x, getDensity_Birt)
}

pBirt <- function(x) {
    calculate()
    sapply(x, getCDF)
}

plotlyBirthdayDistribution <- function(plotrange, input, distType, probrange) {
    popSize <<- as.numeric(input$BirthN)
    sampleSize <<- as.numeric(input$BirthK)
    
    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f9 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f9 <- dBirt(xseq)
        graphtype <- "PMF"
    } else if (input$FunctionType == "CDF/CMF") {
        f9 <- pBirt(xseq)
        graphtype <- "CMF"
    } else {
        graphtype <- ""
    }

    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f9, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f9
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Birt(as.numeric(probrange[2])) - getCDF_Birt(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f9), max(f9))
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
