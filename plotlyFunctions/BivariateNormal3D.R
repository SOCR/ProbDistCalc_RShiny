plotlyBivariateNormal3DDistribution <- function(plotrange, input, distType, probrange) {
    x1 <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 2),
        length.out=500)
    x2 <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 2),
        length.out=500)

    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f <- function(x1,x2) dmnorm(cbind(x1,x2), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))
        f10 <- outer(x1,x2,f)
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f <- function(x1,x2) pmnorm(cbind(x1,x2), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))
        f10 <- outer(x1,x2,f)
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
        prob <- pmnorm(cbind(as.numeric(probrange[2]),as.numeric(probrange[2])), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))
            - pmnorm(cbind(as.numeric(probrange[1]),as.numeric(probrange[1])), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))
            
        fig <- fig %>% add_surface(x = x1, y = x2, z = f10, name = paste("Probability = ", prob, sep = ""),
                hoverinfo = "name")
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
