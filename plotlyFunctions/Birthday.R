popSize <- 0
sampleSize <- 0

prob

calculate <- function() {
    temp <- rep(0, (popSize+1)*(sampleSize+1))

    prob <<- array(temp, dim=c(popSize+1, sampleSize+1))
    prob[1,1] <<- 1
    prob[2,2] <<- 1

    for (i in 2:sampleSize) {
        if (i < popSize + 1) {
            upperIndex <- i + 1
        } else {
            upperIndex <- popSize + 1
        }
        for (j in 2:upperIndex) {
            prob[j,i+1] <<- prob[j, i] * (j/popSize) + prob[j-1, i] * ( (popSize - j + 1) / popSize )
        }
    }
}

getDensity <- function(x) {
    x = round(x)
    if (x<=0) {
        return(0)
    }
    return(prob[x, sampleSize+1])
}

getCDF <- function(x) {
    x = round(x)
    if (x<=0) {
        return(0)
    }

    res <- 0

    for (i in 1:x) {
        res = res + prob[i, sampleSize+1]
    }
    return(res)
}

dBirt <- function(x) {
    calculate()
    sapply(x, getDensity)
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
        xsize = length(xseq)
        colors = c(rep("rgb(31, 119, 180)", xsize))
        for (index in 1:xsize) {
            if (xseq[index] >= round(probrange[1], 0) && xseq[index] <= round(probrange[2],
                0)) {
                colors[index] = "rgb(255, 127, 14)"
            }
        }
        fig <- plot_ly(x = xseq, y = f9, name = distType, type = "bar", marker = list(color = colors),
            text = f9, hovertemplate = paste("<br><b>Prob. </b>: %{y}</br>", "<b>X</b>: %{x}",
                "<b>Y</b>: %{y}"), )
        fig <- fig %>%
            plotly::layout(title = paste(distributions[9], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                  zeroline = TRUE, range = c(min(f9), max(f9)), type = "linear"),
                xaxis = list(showticklabels = TRUE, title = "* All x values rounded to nearest integers",
                  zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                  linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
