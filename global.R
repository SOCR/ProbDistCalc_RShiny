# SOCR Probability Distribution Calculator
# Version 0.9
# Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
# Orginally created by Jared(Tianyi) Chai


# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Global.R ----------------------- #
# For Retrieving and Storing Global Variables
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library(fitdistrplus)
library("Rlab")
library("shinyWidgets")

plotlyFunctions <- list.files("plotlyFunctions", full.names = TRUE)
for (file in plotlyFunctions) {
  source(file)
}
source("fitFunctions.R")
# ----------------------- Parse and Store xml Databse of Metadata from Distributome Project ----------------------- #
xml_data <- read_xml("Distributome.xml", encoding = "UTF-8")
xml_rootnode <- xml_root(xml_data)
xml_distributions <- xml_children(xml_child(xml_rootnode))
xml_len <- length(xml_distributions)
xml_wid <- 0
xml_tags <- c()
for (i in 1:xml_len) {
  xml_i <- xml_contents(xml_distributions[[i]])
  width <- length(xml_i)
  if (width > xml_wid) {
    xml_wid <- width
    xml_tags <- xml_name(xml_i)
  }
}
distributions_meta <- matrix("", xml_wid, xml_len * 2)
for (i in 1:xml_len) {
  xml_i <- xml_contents(xml_distributions[[i]])
  width <- length(xml_i)
  for (j in 1:width) {
    distributions_meta[[j, i * 2 - 1]] <- xml_name(xml_i[j])
    distributions_meta[[j, i * 2]] <- xml_text(xml_i[j])
  }
}

distributionInfoClass <- function(id, name, inputNames, labels, defaultValues, hasImplementation = TRUE, isWithSD = FALSE, fitFunc = NULL) {
  structure(list(
    id = as.integer(id),
    name = name,
    inputNames = inputNames,
    labels = labels,
    defaultValues = defaultValues,
    hasImplementation = hasImplementation,
    isWithSD = isWithSD,
    fitFunc = fitFunc
  ), class = "distributionInfo")
}

distributionInfoList <- list(
  "Anderson-Darling Distribution" = distributionInfoClass(1, "AndersonDarling", c("ADn", "ADNCP"), c("n", "non-centrality parameter"), c(1, 1), hasImplementation = FALSE),
  "ArcSine Distribution" = distributionInfoClass(2, "ArcSine", c("ArcSineA", "ArcSineB"), c("A", "B"), c(1, 2)),
  "Benford Distribution" = distributionInfoClass(3, "Benford", c("Benfn"), c("number of leading digits"), c(1)),
  "Bernoulli Distribution" = distributionInfoClass(4, "Bernoulli", c("BernProb"), c("Probability"), c(0.5)),
  "Beta Distribution" = distributionInfoClass(5, "Beta", c("BetaAlpha", "BetaBeta"), c("Alpha", "Beta"), c(2, 2)),
  "Beta (Generalized) Distribution" = distributionInfoClass(6, "BetaGeneralized", c("BetaGenA", "BetaGenB", "BetaGenC", "BetaGenP"), c("A", "B", "C", "P"), c(3, 2, 1, 5)),
  "Beta-Binomial Distribution" = distributionInfoClass(7, "BetaBinomial", c("BetaBinomN", "BetaBinomU", "BetaBinomV"), c("n", "alpha", "beta"), c(2, 3, 0.2)),
  "Binomial Distribution" = distributionInfoClass(8, "Binomial", c("BinomP", "BinomN"), c("Probability", "n"), c(0.5, 10)),
  "Birthday Distribution" = distributionInfoClass(9, "Birthday", c("BirthN"), c("n"), c(10), hasImplementation = FALSE),
  "Bivariate Normal Distribution (3D)" = distributionInfoClass(10, "BivariateNormal", c("BivaM1", "BivaV1", "BivaM2", "BivaV2", "BivaCov"), c("mean of the first random variable", "variance of the first random variable", "mean for the second random variable", "variance of the second random variable", "covariance of the two random variables"), c(0, 2, 0, 2, -1)),
  "Cauchy Distribution" = distributionInfoClass(11, "Cauchy", c("CauchyX0", "CauchyGamma"), c("x0", "Gamma"), c(1, 1)),
  "Chi Distribution" = distributionInfoClass(12, "Chi", c("ChiK"), c("k"), c(1), isWithSD = TRUE),
  "Chi Square Distribution" = distributionInfoClass(13, "ChiSquare", c("Chi2n"), c("n"), c(1), isWithSD = TRUE),
  "Chi Square Non Central Distribution" = distributionInfoClass(14, "ChiSquareNonCentral", c("Chi2NCn", "Chi2NCNCP"), c("n", "non-centrality paramter"), c(1, 1), isWithSD = TRUE),
  "Circle Distribution" = distributionInfoClass(15, "Circle", c("CircleRadius"), c("radius"), c(2)),
  "Continuous Uniform Distribution" = distributionInfoClass(16, "ContinuousUniform", c("UnifMin", "UnifMax"), c("Min.", "Max."), c(0, 1)),
  "Coupon Distribution" = distributionInfoClass(17, "Coupon", c("CouponN"), c("n"), c(10), hasImplementation = FALSE),
  "Die Distribution" = distributionInfoClass(18, "Die", c("DieN"), c("n"), c(6), hasImplementation = FALSE),
  "Discrete ArcSine Distribution" = distributionInfoClass(19, "DiscreteArcSine", c("DisArcSineA", "DisArcSineB"), c("A", "B"), c(1, 30)),
  "Discrete Uniform Distribution" = distributionInfoClass(20, "DiscreteUniform", c("DisUnifMin", "DisUnifMax"), c("Min.", "Max."), c(0, 1)),
  "Erlang Distribution" = distributionInfoClass(21, "Erlang", c("ErlangScale", "ErlangShape"), c("scale", "shape"), c(1, 1)),
  "Error Distribution" = distributionInfoClass(22, "Error", c("ErrorLocation", "ErrorScale", "ErrorShape"), c("location", "scale", "shape"), c(0, 1, 2)),
  "Exponential Distribution" = distributionInfoClass(23, "Exponential", c("ExpLambda"), c("lambda"), c(1)),
  "Finite Distribution" = distributionInfoClass(24, "Finite", c("FinProb"), c("Probability"), c(0.5), hasImplementation = FALSE),
  "F Distribution" = distributionInfoClass(25, "F", c("FdOne", "FdTwo"), c("d1", "d2"), c(1, 1)),
  "Fishers-Tippett Distribution" = distributionInfoClass(26, "FishersTippett", c("GEVMiu", "GEVSigma", "GEVEpsilon"), c("miu", "sigma", "Epsilon"), c(1, 1, 1), isWithSD = TRUE),
  "Gamma Distribution" = distributionInfoClass(27, "Gamma", c("GammaA", "GammaB"), c("alpha", "beta"), c(1, 1), isWithSD = TRUE),
  "General Cauchy Distribution" = distributionInfoClass(28, "GeneralCauchy", c("GeneralCauchyAlpha", "GeneralCauchyBeta"), c("alpha", "beta"), c(1, 1)),
  "Generalized Extreme Value (GEV) Distribution" = distributionInfoClass(29, "GeneralizedExtremeValueGEV", c("GEVMiu", "GEVSigma", "GEVEpsilon"), c("miu", "sigma", "Epsilon"), c(1, 1, 1)),
  "Geometric Distribution" = distributionInfoClass(30, "Geometric", c("GeomProb"), c("Probability"), c(0.5), isWithSD = TRUE),
  "Gilbrats Distribution" = distributionInfoClass(31, "Gilbrats", c("GilbratsMu", "GilbratsSigma"), c("mu", "sigma"), c(0, 1)),
  "Gompertz Distribution" = distributionInfoClass(32, "Gompertz", c("Gompertz_N", "Gompertz_B"), c("shape eta", "scale Beta"), c(2, 3)),
  "Gumbel Distribution" = distributionInfoClass(33, "Gumbel", c("Gumbel_U", "Gumbel_Beta"), c("location u", "scale Beta"), c(2, 3), isWithSD = TRUE),
  "Half Normal Distribution" = distributionInfoClass(34, "HalfNormal", c("HNorm"), c("sigma"), c(1), isWithSD = TRUE),
  "Hyper Geometric Distribution" = distributionInfoClass(35, "HyperGeometric", c("HyperM", "HyperN", "HyperK"), c("m", "N", "n"), c(50, 500, 100), isWithSD = TRUE),
  "Hyperbolic-Secant Distribution" = distributionInfoClass(36, "HyperbolicSecant", c("HSmu", "HSsigma"), c("mu", "sigma"), c(0, 1)),
  "Inverse-Gamma Distribution" = distributionInfoClass(37, "InverseGamma", c("InvGammaA", "InvGammaB"), c("alpha", "beta"), c(1, 1), isWithSD = TRUE),
  "Inverse-Gaussian(Wald) Distribution" = distributionInfoClass(38, "InverseGaussianWald", c("InvGausM", "InvGausL"), c("mu", "lambda"), c(1, 0.2), isWithSD = TRUE),
  "Johnson SB (Bounded) Distribution" = distributionInfoClass(39, "JohnsonSBBounded", c("JohnSBgamma", "JohnSBdelta", "JohnSBxi", "JohnSBlambda"), c("gamma", "delta", "xi", "lambda"), c(-0.5, 2, -0.5, 2)),
  "Johnson SU (Unbounded) Distribution" = distributionInfoClass(40, "JohnsonSUUnbounded", c("JohnSUgamma", "JohnSUdelta", "JohnSUxi", "JohnSUlambda"), c("gamma", "delta", "xi", "lambda"), c(-0.5, 2, -0.5, 2)),
  "Kolmogorov Distribution" = distributionInfoClass(41, "Kolmogorov", c("Kolmogorovn"), c("n"), c(1), hasImplementation = FALSE),
  "Laplace Distribution" = distributionInfoClass(42, "Laplace", c("LapMu", "LapSig"), c("mu", "sigma"), c(0, 1), isWithSD = TRUE),
  "Logarithmic-series Distribution" = distributionInfoClass(43, "Logarithmicseries", c("LogP"), c("P"), c(0.5), isWithSD = TRUE),
  "Logistic Distribution" = distributionInfoClass(44, "Logistic", c("LogiA", "LogiB"), c("a", "b"), c(0, 1), isWithSD = TRUE),
  "Logistic Exponential Distribution" = distributionInfoClass(45, "LogisticExponential", c("LogEx_A", "LogEx_B"), c("a", "b"), c(5, 1)),
  "LogNormal Distribution" = distributionInfoClass(46, "LogNormal", c("LogNormMean", "LogNormSD"), c("mean", "standard deviation"), c(0, 1), isWithSD = TRUE),
  "Lomax Distribution" = distributionInfoClass(47, "Lomax", c("LomaxLamda", "LomaxKappa"), c("Lambda", "Kappa"), c(1, 2)),
  "Matching Distribution" = distributionInfoClass(48, "Matching", c("MatchParam"), c("parameter"), c(1)),
  "Maxwell Distribution" = distributionInfoClass(49, "Maxwell", c("MaxwellA"), c("a"), c(1), isWithSD = TRUE),
  "Minimax Distribution" = distributionInfoClass(50, "Minimax", c("Mini_B", "Mini_V"), c("Beta", "Gama"), c(1, 2)),
  "Mixture Distribution" = distributionInfoClass(51, "Mixture", c("MixProb"), c("Probability"), c(0.5), hasImplementation = FALSE),
  "Multinomial Distribution" = distributionInfoClass(52, "Multinomial", c("MultProb", "MultN"), c("Probability", "n"), c(0.5, 10), hasImplementation = FALSE),
  "Muth Distribution" = distributionInfoClass(53, "Muth", c("MuthKappa"), c("Kappa"), c(1)),
  "Negative Binomial Distribution" = distributionInfoClass(54, "NegativeBinomial", c("NegBiR", "NegBiP"), c("r", "p"), c(1, 0.5)),
  "Negative HyperGeometric Distribution" = distributionInfoClass(55, "NegativeHyperGeometric", c("NegHyperK", "NegHyperN", "NegHyperR"), c("W", "B", "b"), c(45, 30, 1)),
  "Negative Multinomial Distribution" = distributionInfoClass(56, "NegativeMultinomial", c("NegMultK", "NegMultP"), c("k", "p"), c(1, 0.5), hasImplementation = FALSE),
  "Normal Distribution" = distributionInfoClass(57, "Normal", c("NormMean", "NormSD"), c("mean", "standard deviation"), c(0, 1), isWithSD = TRUE),
  "Normal Truncated Distribution" = distributionInfoClass(58, "NormalTruncated", c("TruncNormMean", "TruncNormSD", "TruncNormMin", "TruncNormMax"), c("mean", "standard deviation", "Min", "Max"), c(0, 3, -5, 5)),
  "Pareto Distribution" = distributionInfoClass(59, "Pareto", c("ParetoA", "ParetoB"), c("a", "b"), c(1, 1)),
  "Point Mass Distribution" = distributionInfoClass(60, "PointMass", c("PMD_Location"), c("location"), c(1)),
  "Poisson Distribution" = distributionInfoClass(61, "Poisson", c("PoiLambda"), c("lambda"), c(1), isWithSD = TRUE),
  "Poker Dice Distribution" = distributionInfoClass(62, "PokerDice", c(), c(), c()),
  "Power Function Distribution" = distributionInfoClass(63, "PowerFunction", c("PowerAlpha", "PowerBeta"), c("alpha", "beta"), c(3, 4)),
  "Rayleigh Distribution" = distributionInfoClass(64, "Rayleigh", c("RayleighSigma"), c("sigma"), c(1), isWithSD = TRUE),
  "Rice Distribution" = distributionInfoClass(65, "Rice", c("RiceVee", "RiceSigma"), c("VEE", "Sigma"), c(1, 1)),
  "T Distribution" = distributionInfoClass(66, "T", c("Tdof"), c("degree of freedom"), c(1)),
  "T Non Central Distribution" = distributionInfoClass(67, "TNonCentral", c("TNCdof", "TNCNCP"), c("degree of freedom", "non-centrality parameter"), c(1, 1)),
  "Triangular Distribution" = distributionInfoClass(68, "Triangular", c("Triangular_A", "Triangular_B", "Triangular_C"), c("a", "b", "c"), c(1, 10, 4)),
  "Two-Sided Power Distribution" = distributionInfoClass(69, "TwoSidedPower", c("TSPowerLeft", "TSPowerRight", "TSPowerMed", "TSPowerPower"), c("left", "right", "med", "power"), c(-5, 5, 0, 3)),
  "U-quadratic distribution" = distributionInfoClass(70, "Uquadratic", c("UQ_C", "UQ_W"), c("c", "w"), c(1, 1)),
  "Von Mises Distribution" = distributionInfoClass(71, "VonMises", c("vonMisesMu", "vonMisesKappa"), c("mu", "kappa"), c(0, 1)),
  "Walk Max Distribution" = distributionInfoClass(72, "WalkMax", c("WalkMaxSteps"), c("steps"), c(4)),
  "Walk Position Distribution" = distributionInfoClass(73, "WalkPosition", c("WalkPosSteps"), c("steps"), c(4), hasImplementation = FALSE),
  "Weibull Distribution" = distributionInfoClass(74, "Weibull", c("WeibullLambda", "WeibullK"), c("lambda", "k"), c(1, 1), isWithSD = TRUE),
  "Zipf-Mandelbrot Distribution" = distributionInfoClass(75, "ZipfMandelbrot", c("Zipf_q", "Zipf_N", "Zipf_s"), c("shape parameter q", "shape parameter N", "shape parameter s"), c(1, 1, 1))
)
distToImpl <- Filter(function(x) !x$hasImplementation, distributionInfoList)
distWithSD <- Filter(function(x) x$isWithSD, distributionInfoList)
distributions <- names(distributionInfoList)
nameToFullName <- function(name) {
  for (distributionInfo in distributionInfoList) {
    if (distributionInfo$name == name) {
      return(distributions[distributionInfo$id])
    }
  }
}
# assign the function named fitFuncName to distributionInfo$fitFunc
for (name in distributions) {
  fitFuncName <- paste0("fit", distributionInfoList[[name]]$name)
  if (exists(fitFuncName)) {
      distributionInfoList[[name]]$fitFunc <- get(fitFuncName)
  }
  else {
      print(paste0("[WARNING] fit function ", fitFuncName, " does not exist"))
  }
}

# library(yaml)
# data_to_save <- lapply(distributionInfoList, as.list)
# write_yaml(data_to_save, "distribution_info.yaml")
