getCDF_Die <- function(x) {
    k <- round(x)
    if (n_Die == 0) {
        # FAIR Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(1/6)
        } else if (k == 2) {
            return(2/6)
        } else if (k == 3) {
            return(3/6)
        } else if (k == 4) {
            return(4/6)
        } else if (k == 5) {
            return(5/6)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    } else if (n_Die == 1) {
        # FLAT16 Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(2/8)
        } else if (k == 2) {
            return(3/8)
        } else if (k == 3) {
            return(4/8)
        } else if (k == 4) {
            return(5/8)
        } else if (k == 5) {
            return(6/8)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    } else if (n_Die == 2) {
        # FLAT25 Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(1/8)
        } else if (k == 2) {
            return(3/8)
        } else if (k == 3) {
            return(4/8)
        } else if (k == 4) {
            return(5/8)
        } else if (k == 5) {
            return(7/8)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    } else if (n_Die == 3) {
        # FLAT34 Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(1/8)
        } else if (k == 2) {
            return(2/8)
        } else if (k == 3) {
            return(4/8)
        } else if (k == 4) {
            return(6/8)
        } else if (k == 5) {
            return(7/8)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    } else if (n_Die == 4) {
        # LEFT Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(1/21)
        } else if (k == 2) {
            return(3/21)
        } else if (k == 3) {
            return(6/21)
        } else if (k == 4) {
            return(10/21)
        } else if (k == 5) {
            return(15/21)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    } else if (n_Die == 5) {
        # RIGHT Die
        if (k < 1) {
            return(0)
        } else if (k == 1) {
            return(6/21)
        } else if (k == 2) {
            return(11/21)
        } else if (k == 3) {
            return(15/21)
        } else if (k == 4) {
            return(18/21)
        } else if (k == 5) {
            return(20/21)
        } else if (k == 6) {
            return(1)
        } else {
            return(1)
        }
    }
}

getDensity_Die <- function(x) {
    k <- round(x)
    if (n_Die == 0) {
        # FAIR Die
        if (k < 1 || k > 6) {
            return(0)
        } else {
            return(1/6)
        }
    } else if (n_Die == 1) {
        # FLAT16 Die
        if (k < 1 || k > 6) {
            return(0)
        } else if (k == 1 || k == 6) {
            return(1/4)
        } else {
            return(1/8)
        }
    } else if (n_Die == 2) {
        # FLAT25 Die
        if (k < 1 || k > 6) {
            return(0)
        } else if (k == 2 || k == 5) {
            return(1/4)
        } else {
            return(1/8)
        }
    } else if (n_Die == 3) {
        # FLAT34 Die
        if (k < 1 || k > 6) {
            return(0)
        } else if (k == 3 || k == 4) {
            return(1/4)
        } else {
            return(1/8)
        }
    } else if (n_Die == 4) {
        # LEFT Die
        if (k < 1 || k > 6) {
            return(0)
        } else if (k == 1) {
            return(1/21)
        } else if (k == 2) {
            return(2/21)
        } else if (k == 3) {
            return(3/21)
        } else if (k == 4) {
            return(4/21)
        } else if (k == 5) {
            return(5/21)
        } else {
            return(6/21)
        }
    } else if (n_Die == 5) {
        # RIGHT Die
        if (k < 1 || k > 6) {
            return(0)
        } else if (k == 1) {
            return(6/21)
        } else if (k == 2) {
            return(5/21)
        } else if (k == 3) {
            return(4/21)
        } else if (k == 4) {
            return(3/21)
        } else if (k == 5) {
            return(2/21)
        } else {
            return(1/21)
        }
    }
}

dDie <- function(x) {
    print("Density")
    sapply(x, getDensity_Die)
}

pDie <- function(x) {
    sapply(x, getCDF_Die)
}

plotlyDieDistribution <- function(plotrange, input, distType, probrange) {
    n_Die <<- as.numeric(input$DieN)
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f18 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f18 <- dDie(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f18 <- pDie(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f18, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f18
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Die(as.numeric(probrange[2])) - getCDF_Die(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f18), max(f18))
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