plotlyBivariateNormal3DDistribution <- function(plotrange, input, distType, probrange) {
    x1 <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 5),
        length=500)
    x2 <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 5),
        length=500)

    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        z <- function(x1,x2){ z <- exp(-(as.numeric(input$BivaV2)*(x1-as.numeric(input$BivaM1))^2+as.numeric(input$BivaV1)*(x2-as.numeric(input$BivaM2))^2-2*as.numeric(input$BivaCov)*(x1-as.numeric(input$BivaM1))*(x2-as.numeric(input$BivaM2)))/(2*(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2)))/(2*pi*sqrt(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2)) }
        f10 <- t(outer(x1,x2,z))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f10 <- pcauchy(xseq, as.numeric(input$CauchyX0), as.numeric(input$CauchyGamma))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        fig <- plot_ly(x=x1,y=x2,z=f10)
        xsize = length(x1)
        newy = f10
        for (index in 1:xsize) {
            if (x1[index] < probrange[1] || x1[index] > probrange[2]) {
                newy[index] = NA
            }
        }
        prob = exp(-(as.numeric(input$BivaV2)*(probrange[2]-as.numeric(input$BivaM1))^2+as.numeric(input$BivaV1)*(probrange[2]-as.numeric(input$BivaM2))^2-2*as.numeric(input$BivaCov)*(probrange[2]-as.numeric(input$BivaM1))*(probrange[2]-as.numeric(input$BivaM2)))/(2*(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2)))/(2*pi*sqrt(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2)) -
            exp(-(as.numeric(input$BivaV2)*(probrange[1]-as.numeric(input$BivaM1))^2+as.numeric(input$BivaV1)*(probrange[1]-as.numeric(input$BivaM2))^2-2*as.numeric(input$BivaCov)*(probrange[1]-as.numeric(input$BivaM1))*(probrange[1]-as.numeric(input$BivaM2)))/(2*(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2)))/(2*pi*sqrt(as.numeric(input$BivaV1)*as.numeric(input$BivaV2)-as.numeric(input$BivaCov)^2))

        fig <- fig %>% add_surface()
        fig <- fig %>%
            plotly::layout(title = paste(distributions[10], " - ", graphtype, sep = ""),
                    hovermode = "x", hoverlabel = list(namelength = 100), 
                    yaxis = list(title = "x2", showticklabels = TRUE, zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                    linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1], plotrange[2])), 
                    xaxis = list(title = "x1", showticklabels = TRUE, zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                    linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1], plotrange[2])), 
                    showlegend = FALSE,
                    scene = list(domain=list()))
        fig <- fig %>%
            config(editable = FALSE)
        fig
    }
}
