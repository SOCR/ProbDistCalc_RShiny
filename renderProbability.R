renderProbability <- function(input, output, session) {
  output$probability <- renderText({
    distType <- input$Distribution
    plotrange <- c(0, 0)
    probrange <- c(0, 0)
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
      prob <- parcsine(as.numeric(probrange[2]), as.numeric(input$ArcSineA), as.numeric(input$ArcSineB)) - parcsine(as.numeric(probrange[1]), as.numeric(input$ArcSineA), as.numeric(input$ArcSineB))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Benford Distribution ----------------------- #
    else if (distType == distributions[3]) {
      prob <- pBenf(round(as.numeric(probrange[2]), 0), as.numeric(input$Benfn)) - pBenf(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$Benfn))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
    else if (distType == distributions[4]) {
      prob <- pbern(round(as.numeric(probrange[2]), 0), as.numeric(input$BernProb)) - pbern(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$BernProb))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Beta Distribution ----------------------- #
    else if (distType == distributions[5]) {
      prob <- pbeta(as.numeric(probrange[2]), as.numeric(input$BetaAlpha), as.numeric(input$BetaBeta)) - pbeta(as.numeric(probrange[1]), as.numeric(input$BetaAlpha), as.numeric(input$BetaBeta))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Beta(Generalized) Distribution ----------------------- #
    else if (distType == distributions[6]) {
      prob <- pgenbeta(as.numeric(probrange[2]), as.numeric(input$BetaGenA), as.numeric(input$BetaGenB), as.numeric(input$BetaGenC), as.numeric(input$BetaGenP)) - pgenbeta(as.numeric(probrange[1]), as.numeric(input$BetaGenA), as.numeric(input$BetaGenB), as.numeric(input$BetaGenC), as.numeric(input$BetaGenP))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Beta Binomial Distribution ----------------------- #
    else if (distType == distributions[7]) {
      prob <- pbbinom(round(as.numeric(probrange[2]), 0), as.numeric(input$BetaBinomN), as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV)) - pbbinom(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$BetaBinomN), as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Binomial Distribution ----------------------- #
    else if (distType == distributions[8]) {
      prob <- pbinom(round(as.numeric(probrange[2]), 0), as.numeric(input$BinomN), as.numeric(input$BinomP)) - pbinom(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$BinomN), as.numeric(input$BinomP))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Bivariate Normal Distribution (3D) ----------------------- #
    else if (distType == distributions[10]) {
      prob <- pmnorm(cbind(as.numeric(probrange[2]), as.numeric(probrange[2])), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))
      -pmnorm(cbind(as.numeric(probrange[1]), as.numeric(probrange[1])), c(as.numeric(input$BivaM1), as.numeric(input$BivaM2)), matrix(c(as.numeric(input$BivaV1), as.numeric(input$BivaCov), as.numeric(input$BivaCov), as.numeric(input$BivaV2)), 2))

      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Cauchy Distribution ----------------------- #
    else if (distType == distributions[11]) {
      prob <- pcauchy(as.numeric(probrange[2]), as.numeric(input$CauchyX0), as.numeric(input$CauchyGamma)) - pcauchy(as.numeric(probrange[1]), as.numeric(input$CauchyX0), as.numeric(input$CauchyGamma))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Chi Distribution ----------------------- #
    else if (distType == distributions[12]) {
      prob <- pchi(as.numeric(probrange[2]), as.numeric(input$ChiK)) - pchi(as.numeric(probrange[1]), as.numeric(input$ChiK))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Chi Square Distribution ----------------------- #
    else if (distType == distributions[13]) {
      prob <- pchisq(as.numeric(probrange[2]), as.numeric(input$Chi2n)) - pchisq(as.numeric(probrange[1]), as.numeric(input$Chi2n))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Chi Square Non Central Distribution ----------------------- #
    else if (distType == distributions[14]) {
      prob <- pt(as.numeric(probrange[2]), as.numeric(input$TNCdof), as.numeric(input$TNCNCP)) - pt(as.numeric(probrange[1]), as.numeric(input$TNCdof), as.numeric(input$TNCNCP))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Continuous Uniform Distribution ----------------------- #
    else if (distType == distributions[16]) {
      prob <- punif(as.numeric(probrange[2]), as.numeric(input$UnifMin), as.numeric(input$UnifMax)) - punif(as.numeric(probrange[1]), as.numeric(input$UnifMin), as.numeric(input$UnifMax))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Discrete ArcSine Distribution ----------------------- #
    else if (distType == distributions[19]) {
      prob <- parcsine(round(as.numeric(probrange[2]), 0), as.numeric(input$DisArcSineA), as.numeric(input$DisArcSineB)) - parcsine(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$DisArcSineA), as.numeric(input$DisArcSineB))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Discrete Uniform Distribution ----------------------- #
    else if (distType == distributions[20]) {
      prob <- punif(round(as.numeric(probrange[2]), 0), as.numeric(input$DisUnifMin), as.numeric(input$DisUnifMax)) - punif(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$DisUnifMin), as.numeric(input$DisUnifMax))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Exponential Distribution ----------------------- #
    else if (distType == distributions[23]) {
      prob <- Rlab::pexp(as.numeric(probrange[2]), as.numeric(input$ExpLambda)) - Rlab::pexp(as.numeric(probrange[1]), as.numeric(input$ExpLambda))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: F Distribution ----------------------- #
    else if (distType == distributions[25]) {
      prob <- pf(as.numeric(probrange[2]), as.numeric(input$FdOne), as.numeric(input$FdTwo)) - pf(as.numeric(probrange[1]), as.numeric(input$FdOne), as.numeric(input$FdTwo))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Gamma Distribution ----------------------- #
    else if (distType == distributions[27]) {
      prob <- Rlab::pgamma(as.numeric(probrange[2]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB)) - Rlab::pgamma(as.numeric(probrange[1]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Geometric Distribution ----------------------- #
    else if (distType == distributions[30]) {
      prob <- pgeom(round(as.numeric(probrange[2]), 0), as.numeric(input$GeomProb)) - pgeom(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$GeomProb))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Gompertz Distribution ----------------------- #
    else if (distType == distributions[32]) {
      prob <- pgompertz(as.numeric(plotrange[2]), as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B)) - pgompertz(as.numeric(plotrange[1]), as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Gumbel Distribution ----------------------- #
    else if (distType == distributions[33]) {
      prob <- pgumbel(as.numeric(probrange[2]), as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta)) - pgumbel(as.numeric(probrange[1]), as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Half Normal Distribution ----------------------- #
    else if (distType == distributions[34]) {
      prob <- phnorm(as.numeric(probrange[2]), as.numeric(input$HNorm)) - phnorm(as.numeric(probrange[1]), as.numeric(input$HNorm))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Hyper Geometric Distribution ----------------------- #
    else if (distType == distributions[35]) {
      prob <- phyper(round(as.numeric(probrange[2]), 0), as.numeric(input$HyperM), as.numeric(input$HyperN) - as.numeric(input$HyperM), as.numeric(input$HyperK)) - phyper(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$HyperM), as.numeric(input$HyperN) - as.numeric(input$HyperM), as.numeric(input$HyperK))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Hyperbolic-Secant Distribution ----------------------- #
    else if (distType == distributions[36]) {
      prob <- psech(as.numeric(probrange[2]), as.numeric(input$HSmu), as.numeric(input$HSsigma)) -
        psech(as.numeric(probrange[1]), as.numeric(input$HSmu), as.numeric(input$HSsigma))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Inverse Gamma Distribution ----------------------- #
    else if (distType == distributions[37]) {
      prob <- pinvgamma(as.numeric(probrange[2]), as.numeric(input$InvGammaA), 1 / as.numeric(input$InvGammaB)) - pinvgamma(as.numeric(probrange[1]), as.numeric(input$InvGammaA), 1 / as.numeric(input$InvGammaB))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Inverse Gaussian(Wald) Distribution ----------------------- #
    else if (distType == distributions[38]) {
      prob <- pinvgauss(as.numeric(probrange[2]), as.numeric(input$InvGausM), as.numeric(input$InvGausL)) - pinvgauss(as.numeric(probrange[1]), as.numeric(input$InvGausM), as.numeric(input$InvGausL))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Johnson SB (Bounded) Distribution ----------------------- #
    else if (distType == distributions[39]) {
      prob <- pJohnsonSB(as.numeric(probrange[2]), as.numeric(input$JohnSBgamma), as.numeric(input$JohnSBdelta), as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda)) - pJohnsonSB(as.numeric(probrange[1]), as.numeric(input$JohnSBgamma), as.numeric(input$JohnSBdelta), as.numeric(input$JohnSBxi), as.numeric(input$JohnSBlambda))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Johnson SU (Unbounded) Distribution ----------------------- #
    else if (distType == distributions[40]) {
      prob <- pJohnsonSU(as.numeric(probrange[2]), as.numeric(input$JohnSUgamma), as.numeric(input$JohnSUdelta), as.numeric(input$JohnSUxi), as.numeric(input$JohnSUlambda)) - pJohnsonSU(as.numeric(probrange[1]), as.numeric(input$JohnSUgamma), as.numeric(input$JohnSUdelta), as.numeric(input$JohnSUxi), as.numeric(input$JohnSUlambda))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Laplace Distribution ----------------------- #
    else if (distType == distributions[42]) {
      prob <- plaplace(as.numeric(probrange[2]), as.numeric(input$LapMu), as.numeric(input$LapSig)) - plaplace(as.numeric(probrange[1]), as.numeric(input$LapMu), as.numeric(input$LapSig))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Logarithmic Series Distribution ----------------------- #
    else if (distType == distributions[43]) {
      prob <- 0
      for (i in (round(as.numeric(probrange[1])):round(as.numeric(probrange[2])))) {
        prob <- prob + dlogseries(i, as.numeric(input$LogP))
      }
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Logistic Distribution ----------------------- #
    else if (distType == distributions[44]) {
      prob <- plogis(as.numeric(probrange[2]), as.numeric(input$LogiA), as.numeric(input$LogiB)) - plogis(as.numeric(probrange[1]), as.numeric(input$LogiA), as.numeric(input$LogiB))
      paste("Prob. = ", prob, sep = "")
    }
    #-----------------------Continuous: Logistic-Exponential Distribution ----------------------- #
    else if (distType == distributions[45]) {
      prob <- 0
      Beta <- as.numeric(input$LogEx_B)
      Alpha <- as.numeric(input$LogEx_A)
      if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
        prob <- 0
      } else if (as.numeric(probrange[1]) <= 0) {
        prob <- ((exp(Alpha * (as.numeric(probrange[2]))) - 1)**Beta) / (1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1)**Beta)
      } else if (as.numeric(probrange[1]) > 0 && as.numeric(probrange[2]) > 0) {
        prob <- ((exp(Alpha * (as.numeric(probrange[2]))) - 1)**Beta) / (1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1)**Beta)
        -((exp(Alpha * (as.numeric(probrange[1]))) - 1)**Beta) / (1 + (exp(Alpha * (as.numeric(probrange[1]))) - 1)**Beta)
      }
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: LogNormal Distribution ----------------------- #
    else if (distType == distributions[46]) {
      prob <- plnorm(as.numeric(probrange[2]), as.numeric(input$LogNormMean), as.numeric(input$LogNormSD)) - plnorm(as.numeric(probrange[1]), as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Lomax Distribution ----------------------- #
    else if (distType == distributions[47]) {
      prob <- plomax(as.numeric(probrange[2]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa)) - plomax(as.numeric(probrange[1]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Maxwell Distribution ----------------------- #
    else if (distType == distributions[49]) {
      prob <- pmaxwell(as.numeric(probrange[2]), as.numeric(input$MaxwellA)) - pmaxwell(as.numeric(probrange[1]), as.numeric(input$MaxwellA))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Minimax Distribution ----------------------- #
    else if (distType == distributions[50]) {
      prob <- 0
      Beta <- as.numeric(input$Mini_B)
      Gamma <- as.numeric(input$Mini_V)
      if (as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) <= 0) {
        prob <- 1
      } else if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
        prob <- 0
      } else if (as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) >= 1) {
        prob <- 1
      } else if (as.numeric(probrange[2]) >= 1) {
        prob <- 1 - (1 - (1 - (as.numeric(probrange[1]))**Beta)**(Gamma))
      } else if (as.numeric(probrange[1]) <= 0) {
        prob <- 1 - (1 - (as.numeric(probrange[2]))**Beta)**(Gamma)
      } else {
        prob <- 1 - (1 - (as.numeric(probrange[2]))**Beta)**(Gamma) - (1 - (1 - (as.numeric(probrange[1]))**Beta)**(Gamma))
      }
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Muth Distribution ----------------------- #
    else if (distType == distributions[53]) {
      K <- as.numeric(input$MuthKappa)
      prob <- 0
      if (as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0) {
        prob <- 0
      } else if (as.numeric(probrange[2]) <= 0) {
        prob <- 0
      } else if (as.numeric(probrange[1]) <= 0) {
        prob <- 1 - exp(-(exp(K * as.numeric(probrange[2]))) / K + K * as.numeric(probrange[2]) + 1 / K)
      } else {
        prob <- (1 - exp(-(exp(K * as.numeric(probrange[2]))) / K + K * as.numeric(probrange[2]) + 1 / K)) - (1 - exp(-(exp(K * as.numeric(probrange[1]))) / K + K * as.numeric(probrange[1]) + 1 / K))
      }

      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Negatvie Binomial Distribution ----------------------- #
    else if (distType == distributions[54]) {
      prob <- pnbinom(round(as.numeric(probrange[2]), 0), as.numeric(input$NegBiR), as.numeric(input$NegBiP)) - pnbinom(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$NegBiR), as.numeric(input$NegBiP))

      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Negatvie HyperGeometric Distribution ----------------------- #
    else if (distType == distributions[55]) {
      prob <- pnhyper(round(as.numeric(probrange[2]), 0), as.numeric(input$NegHyperK) - as.numeric(input$NegHyperN), as.numeric(input$NegHyperN), as.numeric(input$NegHyperR)) - pnhyper(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$NegHyperK) - as.numeric(input$NegHyperN), as.numeric(input$NegHyperN), as.numeric(input$NegHyperR))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Normal Distribution ----------------------- #
    else if (distType == distributions[57]) {
      prob <- pnorm(as.numeric(probrange[2]), as.numeric(input$NormMean), as.numeric(input$NormSD)) - pnorm(as.numeric(probrange[1]), as.numeric(input$NormMean), as.numeric(input$NormSD))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Normal Truncated Distribution ----------------------- #
    else if (distType == distributions[58]) {
      prob <- ptruncnorm(as.numeric(probrange[2]), as.numeric(input$TruncNormMin), as.numeric(input$TruncNormMax), as.numeric(input$TruncNormMean), as.numeric(input$TruncNormSD)) - ptruncnorm(as.numeric(probrange[1]), as.numeric(input$TruncNormMin), as.numeric(input$TruncNormMax), as.numeric(input$TruncNormMean), as.numeric(input$TruncNormSD))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Pareto Distribution ----------------------- #
    else if (distType == distributions[59]) {
      prob <- ppareto(as.numeric(probrange[2]), as.numeric(input$ParetoA), as.numeric(input$ParetoB)) - ppareto(as.numeric(probrange[1]), as.numeric(input$ParetoA), as.numeric(input$ParetoB))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Poisson Distribution ----------------------- #
    else if (distType == distributions[61]) {
      prob <- ppois(round(as.numeric(probrange[2]), 0), as.numeric(input$PoiLambda)) - ppois(round(as.numeric(probrange[1]), 0) - 1, as.numeric(input$PoiLambda))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Power Function Distribution ----------------------- #
    else if (distType == distributions[63]) {
      prob <- ppower(round(as.numeric(probrange[2]), 0), as.numeric(input$PowerAlpha), as.numeric((input$PowerBeta))) - ppower(round(as.numeric(probrange[1]), 0), as.numeric(input$PowerAlpha), as.numeric((input$PowerBeta)))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Rayleigh Distribution ----------------------- #
    else if (distType == distributions[64]) {
      prob <- prayleigh(as.numeric(probrange[2]), as.numeric(input$RayleighSigma)) - prayleigh(as.numeric(probrange[1]), as.numeric(input$RayleighSigma))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Rice Distribution -------------------- #
    else if (distType == distributions[65]) {
      prob <- price(as.numeric(probrange[2]), as.numeric(input$RiceSigma), as.numeric(input$RiceVee)) - Rlab::pweibull(as.numeric(probrange[1]), as.numeric(input$RiceSigma), as.numeric(input$RiceVee))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: T Distribution ----------------------- #
    else if (distType == distributions[66]) {
      prob <- pt(as.numeric(probrange[2]), as.numeric(input$Tdof)) - pt(as.numeric(probrange[1]), as.numeric(input$Tdof))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: T Non Central Distribution ----------------------- #
    else if (distType == distributions[67]) {
      prob <- pt(as.numeric(probrange[2]), as.numeric(input$TNCdof), as.numeric(input$TNCNCP)) - pt(as.numeric(probrange[1]), as.numeric(input$TNCdof), as.numeric(input$TNCNCP))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Triangular Distribution ----------------------- #
    else if (distType == distributions[68]) {
      prob <- ptriangular(as.numeric(probrange[2]), as.numeric(input$Triangular_A), as.numeric(input$Triangular_B), as.numeric(input$Triangular_C)) - ptriangular(as.numeric(probrange[1]), as.numeric(input$Triangular_A), as.numeric(input$Triangular_B), as.numeric(input$Triangular_C))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: U-quadratic distribution ----------------------- #
    else if (distType == distributions[70]) {
      prob <- 0
      W <- as.numeric(input$UQ_W)
      C <- as.numeric(input$UQ_C)
      if (as.numeric(probrange[2]) >= W + C && as.numeric(probrange[1] <= C - W)) {
        prob <- 1
      } else if (as.numeric(probrange[2]) <= C - W && as.numeric(probrange[1] <= C - W)) {
        prob <- 0
      } else if (as.numeric(probrange[2]) >= W + C) {
        prob <- 1 - 0.5 * (1 + ((as.numeric(probrange[1]) - C) / W)**3)
      } else if (as.numeric(probrange[1] <= C - W)) {
        prob <- 0.5 * (1 + ((as.numeric(probrange[2]) - C) / W)**3)
      } else {
        prob <- 0.5 * (1 + ((as.numeric(probrange[2]) - C) / W)**3) - 0.5 * (1 + ((as.numeric(probrange[1]) - C) / W)**3)
      }
      paste("Prob. = ", prob, sep = "")
    } else if (distType == distributions[71]) {
      prob <- circular::pvonmises(as.numeric(probrange[2]), as.numeric(input$vonMisesMu), as.numeric(input$vonMisesKappa)) -
        circular::pvonmises(as.numeric(probrange[1]), as.numeric(input$vonMisesMu), as.numeric(input$vonMisesKappa))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Continuous: Weibull Distribution ----------------------- #
    else if (distType == distributions[74]) {
      prob <- Rlab::pweibull(as.numeric(probrange[2]), as.numeric(input$WeibullK), as.numeric(input$WeibullLambda)) - Rlab::pweibull(as.numeric(probrange[1]), as.numeric(input$WeibullK), as.numeric(input$WeibullLambda))
      paste("Prob. = ", prob, sep = "")
    }
    # ----------------------- Discrete: Zipf-Mandelbrot Distribution  -------------------- #
    else if (distType == distributions[75]) {
      prob <- pzipfman(round(as.numeric(probrange[2]), 0), as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N)) - pzipfman(round(as.numeric(probrange[1]), 0), as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N))
      paste("Prob. = ", prob, sep = "")
    }
  })
}
