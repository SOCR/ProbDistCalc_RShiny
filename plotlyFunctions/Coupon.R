getMean_Coupon <- function() {
    sum <- 0
    for (i in 1:distNum_Coupon) {
        sum <- sum + ( as.numeric(popSize_Coupon) / ( as.numeric(popSize_Coupon) - i + 1 ))
    }
    return(sum)
}

getSD_Coupon <- function() {
    SD <- sqrt(getVariance_Coupon())
    return(SD)
}

getVariance_Coupon <- function() {
    sum <- 0
    for (i in 1:distNum_Coupon) {
        sum <- sum + (as.numeric(popSize_Coupon) * (i - 1)) / ( (popSize_Coupon - i + 1) * (popSize_Coupon - i + 1) )
    }
    return(sum)
}

calculate_Coupon <- function() {
    upper_Coupon <<- ceiling(getMean_Coupon() + 4 * getSD_Coupon())

    temp <- rep(0, (upper_Coupon+1)*(popSize_Coupon+1))

    prob_Coupon <<- array(temp, dim=c(upper_Coupon+1, popSize_Coupon+1))
    prob_Coupon[1,1] <<- 1
    prob_Coupon[2,2] <<- 1

    upperIndex_Coupon <- 0

    for (i in 1:(upper_Coupon-1)) {
        if (i < popSize_Coupon) {
            upperIndex_Coupon <- i + 1
        } else {
            upperIndex_Coupon <- popSize_Coupon
        }

        for (j in 1:upperIndex_Coupon) {
            prob_Coupon[i+2, j+1] <<- prob_Coupon[i+1, j+1] * (j / popSize_Coupon) + prob_Coupon[i+1, j] * ((popSize_Coupon - j + 1) / popSize_Coupon)
        }
    }
}

getDensity_Coupon <- function(x) {
    k <- round(x)
    if (k < distNum_Coupon || k > upper_Coupon) {
        return(0)
    } else {
        res <- ( (popSize_Coupon - distNum_Coupon + 1) / popSize_Coupon ) * (prob_Coupon[k,distNum_Coupon])
        return(res)
    }
}

getCDF_Coupon <- function(x) {
    k <- round(x)
    
    if (k < distNum_Coupon) {
        return(0)
    } else if (k > upper_Coupon) {
        return(1)
    } else {
        res <- 0

        for (i in 1:k) {
            res = res + ( (popSize_Coupon - distNum_Coupon + 1) / popSize_Coupon ) * (prob_Coupon[i,distNum_Coupon])
        }

        return(res)
    }
}

dCoupon <- function(x) {
    sapply(x, getDensity_Coupon)
}

pCoupon <- function(x) {
    sapply(x, getCDF_Coupon)
}

plotlyCouponDistribution <- function(plotrange, input, distType, probrange) {
    popSize_Coupon <<- as.numeric(input$CouponPopSize)
    distNum_Coupon <<- as.numeric(input$CouponDistNum)
    calculate_Coupon()
    xseq <- seq(
        min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01
    )
    f17 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f17 <- dCoupon(xseq)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f17 <- pCoupon(xseq)
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(
            x = xseq, y = f17, name = distType, type = "scatter", mode = "lines",
            hoverinfo = "xy"
        )
        xsize <- length(xseq)
        newy <- f17
        for (index in 1:xsize) {
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
                newy[index] <- NA
            }
        }
        prob <- getCDF_Coupon(as.numeric(probrange[2])) - getCDF_Coupon(as.numeric(probrange[1]))
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
                    zeroline = TRUE, range = c(min(f17), max(f17))
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