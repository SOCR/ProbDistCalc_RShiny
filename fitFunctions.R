fitBeta <- function(dataset) {
    scaled_data <- (dataset - min(dataset)) / (max(dataset) - min(dataset))
    fitDistModel <- fitdist(scaled_data, "beta", method = "mme")
    return(fitDistModel)
}

fitNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "norm")
    return(fitDistModel)
}


fitExponential <- function(dataset) {
    fitDistModel <- fitdist(dataset, "exp")
    return(fitDistModel)
}
