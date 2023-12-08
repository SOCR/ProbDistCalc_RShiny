getIndex_Finite <- function(x) {
    if (x < lower_Finite) {
        return(-1)
    }

    if (x > upper_Finite) {
        return(size_Finite)
    }

    return(round( ( x - lower_Finite ) / width_Finite ))
}

calculate_Finite <- function(a, b, w) {
    a <- a - 0.5 * w
    b <- b + 0.5 * w
    w <- w

    if (w <= 0) {
        w <- 1
    }

    width_Finite <<- w

    if (b < a) {
        b <- a
    }

    lower_Finite <<- a - 0.5 * width_Finite
    upper_Finite <<- b + 0.5 * width_Finite

    size_Finite <<- round( (upper_Finite - lower_Finite) / width_Finite )

    prob_Finite <<- rep(1/size_Finite, size_Finite)
}

getDensity_Finite <- function(x) {
    j <- getIndex_Finite(x)
    
    if ( 0 <= j && j < size_Finite ) {
        return(prob_Finite[j+1])
    } else {
        return(0)
    }
}

getCDF_Finite <- function(x) {
    j <- getIndex_Finite(x)
    res <- 0

    if (j < 0) {
        return(0)
    } else if (j >= size_Finite) {
        return(1)
    } else {
        for (i in 0:j) {
            res <- res + prob_Finite[i+1]
        }
        return(res)
    }
}

dFinite <- function(x) {
    sapply(x, getDensity_Finite)
}

pFinite <- function(x) {
    sapply(x, getCDF_Finite)
}

plotlyFiniteDistribution <- function(plotrange, input, distType, probrange) {
    calculate_Finite(as.numeric(input$FiniteA), as.numeric(input$FiniteB), as.numeric(input$FiniteW))
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f24 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f24 <- dFinite(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f24 <- pFinite(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f24, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f24
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Finite(as.numeric(probrange[2])) - getCDF_Finite(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f24), max(f24))
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