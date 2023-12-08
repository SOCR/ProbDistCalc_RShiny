correct_mult <- function() {
    if (n_multinomial < 0) {
        n_multinomial <<- 1
    }

    if (length(prob_multinomial) < 2 || length(x_multinomial) < 2 || length(prob_multinomial) != length(x_multinomial)) {
        prob_multinomial <<- c(0.5, 0.5)
        x_multinomial <<- c(1, 1)
        n_multinomial <<- 2
    }

    k <- length(prob_multinomial)

    total_prob <- 0
    total_outcome <- 0

    for (i in 1:k) {
        if (prob_multinomial[i] < 0.0 || prob_multinomial[i] > 1.0) {
            prob_multinomial[i] <<- 0.0
        }

        total_prob <- total_prob + prob_multinomial[i]

        if (x_multinomial[i] < 0 || x_multinomial[i] > n_multinomial) {
            x_multinomial[i] <<- 0
        }

        total_outcome <- total_outcome + x_multinomial[i]
    }

    if (total_prob != 1 || total_outcome != n_multinomial) {
        prob_multinomial <<- c(0.5, 0.5)
        x_multinomial <<- c(1, 1)
        n_multinomial <<- 2
    }
}

getPMF <- function(x) {
    dmultinom(x=x_multinomial, prob=prob_multinomial)
}

dMultinomial <- function(x) {
    sapply(x, getPMF)
}

getCDF <- function(x) {
    0
}

pMultinomial <- function(x) {
    sapply(x, getCDF)
}

plotlyMultinomialDistribution <- function(plotrange, input, distType, probrange) {
    prob_multinomial <<- as.numeric(unlist(strsplit(input$MultProb,",")))
    x_multinomial <<- as.numeric(unlist(strsplit(input$MultX,",")))
    n_multinomial <<- as.numeric(input$MultN)

    correct_mult()

    xseq <- seq(round(min(0, as.numeric(plotrange[1])), 0), round(max(
        as.numeric(plotrange[2]),
        10
    ), 0), 1)
    f52 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f52 <- dMultinomial(xseq)
        graphtype <- "PMF"
    } else if (input$FunctionType == "CDF/CMF") {
        f52 <- pMultinomial(xseq)
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
            x = xseq, y = f52, name = distType, type = "bar", marker = list(color = colors),
            text = f52, hovertemplate = paste(
                "<br><b>Prob. </b>: %{y}</br>", "<b>X</b>: %{x}",
                "<b>Y</b>: %{y}"
            ),
        )
        fig <- fig %>%
            plotly::layout(
                title = paste(distributions[54], " - ", graphtype, sep = ""),
                hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(
                    fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f52), max(f52)), type = "linear"
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
