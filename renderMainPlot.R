renderMainPlot <- function(input, output, session) {
    output$myPlot <- renderPlotly({
        distType <- input$Distribution
        plotrange <- c(0, 0)
        probrange <- c(0, 0)
        old_SD <- 0
        if (input$Distribution %in% SDS) {

        } else {
            updateSliderInput(
                session,
                "plotrange",
                label = NULL,
                value = NULL,
                min = -1000,
                max = 1000,
                step = NULL,
                timeFormat = NULL,
                timezone = NULL
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
        # ----------------------- Continuous: ArcSine Distribution ----------------------- #
        if (distType == distributions[2]) {
            plotlyArcSineDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Benford Distribution ----------------------- #
        else if (distType == distributions[3]) {
            plotlyBenfordDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
        else if (distType == distributions[4]) {
            plotlyBernoulliDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Beta Distribution ----------------------- #
        else if (distType == distributions[5]) {
            plotlyBetaDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Beta (Generalized) Distribution ----------------------- #
        else if (distType == distributions[6]) {
            plotlyBetaGeneralizedDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Beta-Binomial Distribution ----------------------- #
        else if (distType == distributions[7]) {
            plotlyBetaBinomialDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Binormial Distribution ----------------------- #
        else if (distType == distributions[8]) {
            plotlyBinomialDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Cauchy Distribution ----------------------- #
        else if (distType == distributions[11]) {
            plotlyCauchyDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Chi Distribution ----------------------- #
        else if (distType == distributions[12]) {
            plotlyChiDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Chi Square Distribution ----------------------- #
        else if (distType == distributions[13]) {
            plotlyChiSquareDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Chi Square Non Central Distribution ----------------------- #
        else if (distType == distributions[14]) {
            plotlyChiSqureNonCentralDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Continuous Uniform Distribution ----------------------- #
        else if (distType == distributions[16]) {
            plotlyContinuousUniformDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Discrete ArcSine Distribution ----------------------- #
        else if (distType == distributions[19]) {
            plotlyDiscreteArcSineDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Discrete Uniform Distribution ----------------------- #
        else if (distType == distributions[20]) {
            plotlyDiscreteUniformDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Exponential Distribution ----------------------- #
        else if (distType == distributions[23]) {
            plotlyExponentialDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: F Distribution ----------------------- #
        else if (distType == distributions[25]) {
            plotlyFisherFDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Gamma Distribution ----------------------- #
        else if (distType == distributions[27]) {
            plotlyGammaDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Discrete: GEV Distribution ------------------------------#
        ## Miu = locationn
        ## Sig = scale
        ## Epsilon = shape
        ## dgev(x, loc=0, scale=1, shape=0, log = FALSE)
        ## pgev(q, loc=0, scale=1, shape=0, lower.tail = TRUE)
        else if (distType == distributions[29]) {
            plotlyGeneralizedExtremeValueGEVDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Geometric Distribution ----------------------- #
        else if (distType == distributions[30]) {
            plotlyGeometricDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Gompertz Distribution ----------------------- #
        else if (distType == distributions[32]) {
            plotlyGompertzDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Gumbel Distribution ----------------------- #
        else if (distType == distributions[33]) {
            plotlyGumbelDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Half Normal Distribution ----------------------- #
        else if (distType == distributions[34]) {
            plotlyHalfNormalDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Discrete: Hyper Geometric Distribution ----------------------- #
        else if (distType == distributions[35]) {
            plotlyHyperGeometricDistribution(plotrange, input, distType, probrange, session, old_SD)
        }
        # ----------------------- Continuous: Inverse Gamma Distribution ----------------------- #
        else if (distType == distributions[37]) {
            plotlyInverseGammaDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Inverse Gaussian(Wald) Distribution ----------------------- #
        else if (distType == distributions[38]) {
            plotlyInverseGaussianWaldDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Laplace Distribution ----------------------- #
        else if (distType == distributions[42]) {
            plotlyLaplaceDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Discrete: Logarithmic Series Distribution ----------------------- #
        else if (distType == distributions[43]) {
            plotlyLogarithmicSeriesDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Logistic Distribution ----------------------- #
        else if (distType == distributions[44]) {
            plotlyLogisticDistribution(plotrange, input, distType, probrange, session)
        }
        #-----------------------Continuous: Logistic-Exponential Distribution ----------------------- #
        else if (distType == distributions[45]) {
            plotlyLogisticExponentialDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: LogNormal Distribution ----------------------- #
        else if (distType == distributions[46]) {
            plotlyLogNormalDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Lomax Distribution ----------------------- #
        else if (distType == distributions[47]) {
            plotlyLomaxDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Maxwell Distribution ----------------------- #
        else if (distType == distributions[49]) {
            plotlyMaxwellDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Minimax Distribution ----------------------- #
        else if (distType == distributions[50]) {
            plotlyMinimaxDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Muth Distribution ----------------------- #
        else if (distType == distributions[53]) {
            plotlyMuthDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Negative Binomial Distribution ----------------------- #
        else if (distType == distributions[54]) {
            plotlyNegativeBinomialDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Negative HyperGeometric Distribution ----------------------- #
        else if (distType == distributions[55]) {
            plotlyNegativeHyperGeometricDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Normal Distribution ----------------------- #
        else if (distType == distributions[57]) {
            plotlyNormalDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Normal Truncated Distribution ----------------------- #
        else if (distType == distributions[58]) {
            plotlyNormalTruncatedDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Pareto Distribution ----------------------- #
        else if (distType == distributions[59]) {
            plotlyParetoDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Point Mass Distribution ----------------------- #
        else if (distType == distributions[60]) {
            plotlyPointMassDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Discrete: Poisson Distribution ----------------------- #
        else if (distType == distributions[61]) {
            plotlyPoissonDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Continuous: Rayleigh Distribution ----------------------- #
        else if (distType == distributions[64]) {
            plotlyRayleighDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------- Continuous: Rice Distribution ------------------#

        else if (distType == distributions[65]) {
            plotlyRiceRicianDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: T Distribution ----------------------- #
        else if (distType == distributions[66]) {
            plotlyStudentsTDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: T Non Central Distribution ----------------------- #
        else if (distType == distributions[67]) {
            plotlyStudentsTNonCentralDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Triangular Distribution ----------------------- #
        else if (distType == distributions[68]) {
            plotlyTriangleDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: U-quadratic distribution ----------------------- #
        else if (distType == distributions[70]) {
            plotlyUQuadraticDistribution(plotrange, input, distType, probrange)
        }
        # ----------------------- Continuous: Weibull Distribution ----------------------- #
        else if (distType == distributions[74]) {
            plotlyWeibullDistribution(plotrange, input, distType, probrange, session)
        }
        # ----------------------- Discrete: Zipf-Mandelbrot Distribution ----------------------- #
        else if (distType == distributions[75]) {
            plotlyZipfManelbrotDistribution(plotrange, input, distType, probrange)
        }
    })
}
