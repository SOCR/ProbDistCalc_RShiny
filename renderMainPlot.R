renderMainPlot <- function(input, output, session) {
    output$myPlot <- renderPlotly({
        distType <- input$Distribution
        plotrange <- c(0, 0)
        probrange <- c(0, 0)
        old_SD <- 0
        if (input$Distribution %in% distWithSD) {

        } else {
            updateSliderInput(session, "plotrange",
                label = NULL, value = NULL, min = -1000,
                max = 1000, step = NULL, timeFormat = NULL, timezone = NULL
            )
        }
        if (input$numericalValues == FALSE) {
            plotrange[1] <- input$plotrange[1]
            plotrange[2] <- input$plotrange[2]
            probrange[1] <- input$probrange[1]
            probrange[2] <- input$probrange[2]
        } else {
            plotrange[1] <- input$plotrangeNumMin
            plotrange[2] <- input$plotrangeNumMax
            probrange[1] <- input$probrangeNumMin
            probrange[2] <- input$probrangeNumMax
        }
        distributionInfo = distributionInfoList[[distType]]
        arity = length(formals(distributionInfo$plotlyFunc))
        if ( arity == 4) {
            distributionInfo$plotlyFunc(plotrange, input, distType, probrange)
        } else if (arity == 5){
            distributionInfo$plotlyFunc(plotrange, input, distType, probrange, session)
        } else if (arity == 6){
            distributionInfo$plotlyFunc(plotrange, input, distType, probrange, session, old_SD)
        } else {
            stop("unexpected arity")
        }
    }
    )
}
