n <- 0.0

cdfTerm1 <- function(x, j) {
    T <- (4.0 * j + 1.0) * (4.0 * j + 1.0) * 1.23370055013617 / x
    if (T > 150.0) {
        return(0.0)
    } else {
        a <- 2.22144146907918 * exp(-T) / sqrt(T)
        b <- 3.93740248643060 * 2.0 * pnorm(-sqrt(2 * T), 0, 1)
        r <- x * 0.125
        f <- a + b * r

        for (i in 1:199) {
            c <- ((i - 0.5 - T) * b + T * a) / i
            a <- b
            b <- c
            r <- r * (x / (8 * i + 8))
            if (abs(r) < 1e-40 || abs(c) < 1.e-40) {
                return(f)
            }
            fnew <- f + c * r
            if (f == fnew) {
                return(f)
            }
            f <- fnew
        }

        return(f)
    }
}

cdfTerm2 <- function(x) {
    if (x < 0.01) {
        return(0.0)
    } else {
        r <- 1.0 / x
        term2 <- r * cdfTerm1(x, 0)

        for (j in 1:99) {
            r <- r * ((0.5 - j) / j)
            term2New <- term2 + (4 * j + 1) * r * cdfTerm1(x, j)

            if (term2 == term2New) {
                return(term2)
            } else {
                term2 <- term2New
            }
        }

        return(term2)
    }
}

cdfTerm3 <- function(x) {
    x <- cdfTerm2(x)
    if (x > 0.8) {
        v <- (-130.2137 + (745.2337 - (1705.091 - (1950.646 - (1116.360 - 255.7844 * x) * x) * x) * x) * x) / n
        return(x + v)
    }

    C <- 0.01265 + 0.1757 / n

    if (x < C) {
        v <- x / C
        v <- sqrt(v) * (1.0 - v) * (49 * v - 102)
        return(x + v * (0.0037 / (n * n) + 0.00078 / n + 0.00006) / n)
    }

    v <- (x - C) / (0.8 - C)
    v <- -0.00022633 + (6.54034 - (14.6538 - (14.458 - (8.259 - 1.91864 * v) * v) * v) * v) * v

    return(x + v * (0.04213 + 0.01365 / n) / n)
}

getCDF_n_1 <- function(x) {
    if (x <= 0.38629436111989062) {
        0.0
    } else {
        if (x >= 37.816242111357) {
            1.0
        } else {
            sqrt(1.0 - 4.0 * exp(-x - 1.0))
        }
    }
}

getCDF <- function(x) {
    if (x <= 0.0) {
        return(0.0)
    }
    if (x >= 40.0) {
        return(1.0)
    }
    if (n == 1) {
        return(getCDF_n_1(x))
    }
    cdfValue <- cdfTerm3(x)
    if (cdfValue <= 0.0) {
        return(0.0)
    } else {
        return(cdfValue)
    }
}

specialCaseDensity_n_1 <- function(x) {
    if (x <= 0.38629436111989062 || x >= 37.816242111357) {
        return(0.0)
    }
    t <- exp(-x - 1.0)
    return(2.0 * t / sqrt(1.0 - 4.0 * t))
}

distributionGradient <- function(x, delta) {
    return((getCDF(x + delta) - getCDF(x - delta)) / (2.0 * delta))
}

getDensity <- function(x) {
    if (n <= 0) {
        n <- 1
    }

    if (n == 1) {
        return(specialCaseDensity_n_1(x))
    }

    if (x >= 40 | x <= 0.0) {
        return(0.0)
    }

    delta <- 1.0 / 100.0
    D1 <- distributionGradient(x, delta)
    D2 <- distributionGradient(x, 2.0 * delta)

    densityValue <- D1 + (D1 - D2) / 3.0

    if (densityValue <= 0.0) {
        return(0.0)
    } else {
        return(densityValue)
    }
}

dAD <- function(x, n_in) {
    n <<- n_in
    sapply(x, getDensity)
}

pAD <- function(x, n_in) {
    n <<- n_in
    sapply(x, getCDF)
}

plotlyAndersonDarlingDistribution <- function(plotrange, input, distType, probrange) {
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f1 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f1 <- dAD(xseq, as.numeric(input$ADn))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f1 <- pAD(xseq, as.numeric(input$ADn))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }

    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f1, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f1
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF(as.numeric(probrange[2])) - getCDF(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f1), max(f1))
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
