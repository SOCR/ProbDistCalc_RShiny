nMax_Kol <- 20

densityConst <- function(x) {
    if (x >= 1.0 || x <= 0.5 / degree_Kol) {
        return(0.0)
    }

    if (degree_Kol == 1) {
        return(2.0)
    }

    if (x <= 1.0 / degree_Kol) {
        t <- 2.0 * x - 1.0 / degree_Kol
        
        if (degree_Kol <= nMax_Kol) {
            w <- 2.0 * degree_Kol * factorial(degree_Kol)
            w <- w * (t ^ (degree_Kol - 1))
            return(w)
        }
            
        w <- lgamma(degree_Kol) + (degree_Kol - 1) * log(t)
        return(2 * degree_Kol * exp(w))
    }

    if (x >= 1.0 - 1.0 / degree_Kol) {
        return(2.0 * degree_Kol * ( (1.0 - x) ^ (degree_Kol - 1) ))
    }

    return(-1.0)
}

cdfConst <- function(x) {
    if (degree_Kol * x * x >= 18.0 || x >= 1.0) {
        return(1.0)
    }

    if (x <= 0.5 / degree_Kol) {
        return(0.0)
    }

    if (degree_Kol == 1) {
        return(2.0 * x - 1.0)
    }

    if (x <= 1.0 / degree_Kol) {
        t <- 2.0 * x - 1.0 / degree_Kol

        if (degree_Kol <= nMax_Kol) {
            w <- lgamma(degree_Kol)
            return(w * ( t ^ degree_Kol ))
        }

        w <- lgamma(degree_Kol) + degree_Kol * log(t)
        return(exp(w))
    }

    if (x >= 1.0 - 1.0 / degree_Kol) {
        return(1.0 - 2.0 * ( (1.0 - x) ^ degree_Kol ))
    }

    return(-1.0)
}

mMultiply_Kol <- function(A, B, C, m) {
    print(A)
    for (i in 0:(m-1)) {
        for (j in 0:(m-1)) {
            s <- 0.0
            for (k in 0:(m-1)) {
                s <- s + (A[i * m + k + 1] * B[k * m + j + 1])
            }
            print(s)
            C[i * m + j + 1] <- s
        }
    }

    return(C)
}

mPower_Kol <- function(A, eA, V, m, n) {
    eV <- rep(0,1)
    if (n == 1) {
        for (i in 0:(m * m - 1)) {
            V[i+1] = A[i+1]
        }
        eV[1] = eA
        return(eV)
    }

    eV <- mPower_Kol(A, eA, V, m, n / 2)

    B <- rep(0, m*m)
    B <- mMultiply_Kol(V, V, B, m)
    eB <- 2 * eV[1]

    if (n %% 2 == 0) {
        for (i in 0:(m*m - 1)) {
            V[i+1] = B[i+1]
        }

        eV[1] = eB
    } else {
        V <- mMultiply_Kol(A, B, V, m)
        eV[1] = eA + eB
    }

    if (V[(m / 2) * m + (m / 2) + 1] > 1.0e+140) {
        for (i in 0:(m*m - 1)) {
            V[i+1] <- V[i+1] * 1.0e-140
        }
        eV[1] <- eV[1] + 140
    }

    return(eV)
}

getCDF_Kol <- function(x) {
    cdf_const <- cdfConst(x)
    if (cdf_const != -1.0) {
        return(cdf_const)
    }

    s <- x * x * degree_Kol
    if (x > 7.24 || (s > 3.76 && degree_Kol > 99)) {
        return(1 - 2 * exp(-( 2.000071 + 0.331/sqrt(degree_Kol) + 1.409 / degree_Kol ) * s))
    }

    k <- (degree_Kol * x) + 1
    m <- 2 * k - 1
    h <- k - degree_Kol * x
    H <- rep(0, m*m)
    Q <- rep(0, m*m)

    for (i in 0:(m-1)) {
        for (j in 0:(m-1)) {
            if (i - j + 1 < 0) {
                H[i * m + j + 1] <- 0
            } else {
                H[i * m + j + 1] <- 1
            }
        }
    }

    for (i in 0:(m-1)) {
        H[i * m + 1] <- H[i * m + 1] - ( h ^ (i + 1) )
        H[(m-1)*m + i + 1] <- H[(m-1)*m + i + 1] - ( h ^ (m - i) )
    }

    if (2 * h - 1 > 0) {
        H[(m-1) * m + 1] <- H[(m-1) * m + 1] + ( (2 * h - 1) ^ m )
    }

    for (i in 0:(m-1)) {
        for (j in 0:(m-1)) {
            if (i - j + 1 > 0) {
                for (g in 1:(i-j+1)) {
                    H[i * m + j + 1] <- H[i * m + j + 1] / g
                }
            }
        }
    }

    eH <- 0
    eQ <- (mPower_Kol(H, eH, Q, m, degree_Kol))[1]

    s <- Q[(k - 1) * m + k - 1 + 1]

    for (i in 1:degree_Kol) {
        s <- s * i / degree_Kol

        if (s < 1.0e-140) {
            s <- s * 1.0e+140
            eQ <- eQ - 140
        }
    }

    s <- s * (10.0 ^ eQ)
    return(s)
}

distributionGrad_Kol <- function(x, delta) {
    return( (getCDF_Kol(x + delta) - getCDF_Kol(x - delta)) / (2.0 * delta) )
}

getDensity_Kol <- function(x) {
    densConst <- densityConst(x)
    if (densConst != -1.0) {
        return(densConst)
    }

    delta <- 1.0 / 200.0
    D1 <- distributionGrad_Kol(x, delta)
    D2 <- distributionGrad_Kol(x, 2.0 * delta)

    densConst <- D1 + (D1 - D2) / 3.0

    if (densConst <= 0.0) {
        return(0.0)
    }

    return(densConst)
}

dKol <- function(x) {
    sapply(x, getDensity_Kol)
}

pKol <- function(x) {
    sapply(x, getCDF_Kol)
}

plotlyKolmogorovDistribution <- function(plotrange, input, distType, probrange) {
    degree_Kol <<- as.numeric(input$KolmogorovDegree)
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f41 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f41 <- dKol(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f41 <- pKol(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f41, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f41
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Kol(as.numeric(probrange[2])) - getCDF_Kol(as.numeric(probrange[1]))
        fig <- fig %>%
            add_trace(
                x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)"
            )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[39], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f41), max(f41))
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
