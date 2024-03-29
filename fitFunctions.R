# FIXME: This is not working
# fitAndersonDarling <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "andersonDarling")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitArcSine <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "arcsine")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBenford <- function(dataset) {
#     rounded_data <- round(dataset)
#     fitDistModel <- fitdist(rounded_data, "benford")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBernoulli <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "bernoulli")
#     return(fitDistModel)
# }


fitBeta <- function(dataset) {
    scaled_data <- (dataset - min(dataset)) / (max(dataset) - min(dataset))
    fitDistModel <- fitdist(scaled_data, "beta", method = "mme")
    return(fitDistModel)
}

# FIXME: This is not working
# fitBetaGeneralized <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "betagen")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBetaBinomial <- function(dataset) {
#     rounded_data <- round(dataset)
#     fitDistModel <- fitdist(rounded_data, "betabinom")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBinomial <- function(dataset) {
#     rounded_data <- round(dataset)
#     fitDistModel <- fitdist(rounded_data, "binom")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBirthday <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "birthday")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitBivariateNormal3D <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "bivnorm")
#     return(fitDistModel)
# }

fitCauchy <- function(dataset) {
    fitDistModel <- fitdist(dataset, "cauchy")
    return(fitDistModel)
}

# FIXME: This is not working
# fitChi <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "chi")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitChiSquare <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "chisq")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitChiSquareNonCentral <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "chisqnonc")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitCircle <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "circle")
#     return(fitDistModel)
# }

# Not tested
fitContinuousUniform <- function(dataset) {
    fitDistModel <- fitdist(dataset, "unif")
    return(fitDistModel)
}

# Not tested
fitCoupon <- function(dataset) {
    fitDistModel <- fitdist(dataset, "coupon")
    return(fitDistModel)
}

# Not tested
fitDie <- function(dataset) {
    fitDistModel <- fitdist(dataset, "die")
    return(fitDistModel)
}

# Not tested
fitDiscreteArcSine <- function(dataset) {
    fitDistModel <- fitdist(dataset, "disarc")
    return(fitDistModel)
}

# Not tested
fitDiscreteUniform <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "discunif")
    return(fitDistModel)
}

# FIXME: This is not working
# fitErlang <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "erlang")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitError <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "error")
#     return(fitDistModel)
# }


fitExponential <- function(dataset) {
    fitDistModel <- fitdist(dataset, "exp")
    return(fitDistModel)
}

# FIXME: This is not working
# fitFinite <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "finite")
#     return(fitDistModel)
# }


# FIXME: This is not working
# fitFisherF <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "f")
#     return(fitDistModel)
# }

# Not tested
fitFisherTippett <- function(dataset) {
    fitDistModel <- fitdist(dataset, "fishertippett")
    return(fitDistModel)
}

fitGamma <- function(dataset) {
    fitDistModel <- fitdist(dataset, "gamma")
    return(fitDistModel)
}

# Not tested
fitGeneralCauchy <- function(dataset) {
    fitDistModel <- fitdist(dataset, "gcauchy")
    return(fitDistModel)
}

# Not tested
fitGeneralizedExtremeValueGEV <- function(dataset) {
    fitDistModel <- fitdist(dataset, "gev")
    return(fitDistModel)
}

fitGeometric <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "geom")
    return(fitDistModel)
}

# FIXME: This is not working
# fitGilbrats <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "gilbrat")
#     return(fitDistModel)
# }

# Not tested
fitGompertz <- function(dataset) {
    fitDistModel <- fitdist(dataset, "gompertz")
    return(fitDistModel)
}

# FIXME: This is not working
# fitGumbel <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "gumbel")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitHalfNormal <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "halfnorm")
#     return(fitDistModel)
# }

# FIXME: This is not working
# fitHyperGeometric <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "hyper")
#     return(fitDistModel)
# }

# Not tested
fitHyperbolicSecant <- function(dataset) {
    fitDistModel <- fitdist(dataset, "hyperbolicsecant")
    return(fitDistModel)
}

# FIXME: This is not working
# fitInverseGamma <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "invgamma")
#     return(fitDistModel)
# }

# Not tested
fitInverseGaussianWald <- function(dataset) {
    fitDistModel <- fitdist(dataset, "invgauss")
    return(fitDistModel)
}

# Not tested
fitJohnsonSBBounded <- function(dataset) {
    fitDistModel <- fitdist(dataset, "johnsonsb")
    return(fitDistModel)
}

# Not tested
fitJohnsonSUUnbounded <- function(dataset) {
    fitDistModel <- fitdist(dataset, "johnsonsu")
    return(fitDistModel)
}

# Not tested
fitKolmogorov <- function(dataset) {
    fitDistModel <- fitdist(dataset, "kolmogorov")
    return(fitDistModel)
}

# FIXME: This is not working
# fitLaplace <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "laplace")
#     return(fitDistModel)
# }

# Not tested
fitLogarithmicSeries <- function(dataset) {
    fitDistModel <- fitdist(dataset, "logarithmicseries")
    return(fitDistModel)
}

fitLogistic <- function(dataset) {
    fitDistModel <- fitdist(dataset, "logis")
    return(fitDistModel)
}


# Not tested
fitLogisticExponential <- function(dataset) {
    fitDistModel <- fitdist(dataset, "logisticexponential")
    return(fitDistModel)
}

fitLogNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "lnorm")
    return(fitDistModel)
}

# Not tested
fitLomax <- function(dataset) {
    fitDistModel <- fitdist(dataset, "lomax")
    return(fitDistModel)
}


# Not tested
fitMatching <- function(dataset) {
    fitDistModel <- fitdist(dataset, "matching")
    return(fitDistModel)
}

# Not tested
fitMaxwell <- function(dataset) {
    fitDistModel <- fitdist(dataset, "maxwell")
    return(fitDistModel)
}

# Not tested
fitMinimax <- function(dataset) {
    fitDistModel <- fitdist(dataset, "minimax")
    return(fitDistModel)
}

# Not tested
fitMixture <- function(dataset) {
    fitDistModel <- fitdist(dataset, "mixture")
    return(fitDistModel)
}

# Not tested
fitMultinomial <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "multinom")
    return(fitDistModel)
}

# FIXME: This is not working
# fitMuth <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "muth")
#     return(fitDistModel)
# }

# Not tested
fitNegativeBinomial <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "nbinom")
    return(fitDistModel)
}

# Not tested
fitNegativeHyperGeometric <- function(dataset) {
    fitDistModel <- fitdist(dataset, "neghyper")
    return(fitDistModel)
}

# Not tested
fitNegativeMultinomial <- function(dataset) {
    fitDistModel <- fitdist(dataset, "negmultinom")
    return(fitDistModel)
}

fitNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "norm")
    return(fitDistModel)
}

# FIXME: This is not working
# fitNormalTruncated <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "truncnorm")
#     return(fitDistModel)
# }

# Not tested
fitPareto <- function(dataset) {
    fitDistModel <- fitdist(dataset, "pareto")
    return(fitDistModel)
}

# Not tested
fitPointMass <- function(dataset) {
    fitDistModel <- fitdist(dataset, "pointmass")
    return(fitDistModel)
}

fitPoisson <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "pois", discrete = TRUE)
    return(fitDistModel)
}

# Not tested
fitPokerDice <- function(dataset) {
    fitDistModel <- fitdist(dataset, "pokerdice")
    return(fitDistModel)
}

# FIXME: This is not working
# fitPowerFunction <- function(dataset) {
# fitDistModel <- fitdist(dataset, "powerfunction")
# return(fitDistModel)
# }

# Not tested
fitRayleigh <- function(dataset) {
    fitDistModel <- fitdist(dataset, "rayleigh")
    return(fitDistModel)
}

# Not tested
fitRice <- function(dataset) {
    fitDistModel <- fitdist(dataset, "rice")
    return(fitDistModel)
}

# Not tested
fitStudentsT <- function(dataset) {
    fitDistModel <- fitdist(dataset, "t")
    return(fitDistModel)
}

# Not tested
fitStudentsTNonCentral <- function(dataset) {
    fitDistModel <- fitdist(dataset, "tnoncentral")
    return(fitDistModel)
}

# Not tested
fitTriangular <- function(dataset) {
    fitDistModel <- fitdist(dataset, "triangle")
    return(fitDistModel)
}

# Not tested
fitTwoSidedPower <- function(dataset) {
    fitDistModel <- fitdist(dataset, "twosidedpower")
    return(fitDistModel)
}

# Not tested
fitUQuadratic <- function(dataset) {
    fitDistModel <- fitdist(dataset, "uquadratic")
    return(fitDistModel)
}

# Not tested
fitVonMises <- function(dataset) {
    fitDistModel <- fitdist(dataset, "vonmises")
    return(fitDistModel)
}

# Not tested
fitWalkMax <- function(dataset) {
    fitDistModel <- fitdist(dataset, "walkmax")
    return(fitDistModel)
}

# Not tested
fitWalkPosition <- function(dataset) {
    fitDistModel <- fitdist(dataset, "walkpos")
    return(fitDistModel)
}

# Not tested
fitWeibull <- function(dataset) {
    fitDistModel <- fitdist(dataset, "weibull")
    return(fitDistModel)
}

# Not tested
fitZipfMandelbrot <- function(dataset) {
    fitDistModel <- fitdist(dataset, "zipfmandelbrot")
    return(fitDistModel)
}
