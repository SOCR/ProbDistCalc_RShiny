fitBeta <- function(dataset) {
    scaled_data <- (dataset - min(dataset)) / (max(dataset) - min(dataset))
    fitDistModel <- fitdist(scaled_data, "beta", method = "mme")
    return(fitDistModel)
}

fitExponential <- function(dataset) {
    fitDistModel <- fitdist(dataset, "exp")
    return(fitDistModel)
}

fitNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "norm")
    return(fitDistModel)
}

fitGamma <- function(dataset) {
    fitDistModel <- fitdist(dataset, "gamma")
    return(fitDistModel)
}

fitGeometric <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "geom")
    return(fitDistModel)
}

fitLogistic <- function(dataset) {
    fitDistModel <- fitdist(dataset, "logis")
    return(fitDistModel)
}

fitLogNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "lnorm")
    return(fitDistModel)
}

fitNegativeBinomial <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "nbinom")
    return(fitDistModel)
}

fitPoisson <- function(dataset) {
    rounded_data <- round(dataset)
    fitDistModel <- fitdist(rounded_data, "pois", discrete = TRUE)
    return(fitDistModel)
}
