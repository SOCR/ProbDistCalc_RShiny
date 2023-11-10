library(yaml)

distributionInfoClass <- function(id, name, inputNames, labels, defaultValues, hasImplementation = TRUE, isWithSD = FALSE, fitFunc = NULL, plotlyFunc = NULL) {
  structure(list(
    id = as.integer(id),
    name = name,
    inputNames = inputNames,
    labels = labels,
    defaultValues = defaultValues,
    hasImplementation = hasImplementation,
    isWithSD = isWithSD,
    fitFunc = fitFunc,
    plotlyFunc = plotlyFunc
  ), class = "distributionInfo")
}

loadDistributionInfo <- function(file_path) {
  yaml_data <- read_yaml(file_path)
  parsed_list <- lapply(yaml_data, function(x) {
    distributionInfoClass(
      id = x$id,
      name = x$name,
      inputNames = x$inputNames,
      labels = x$labels,
      defaultValues = x$defaultValues,
      hasImplementation = x$hasImplementation,
      isWithSD = x$isWithSD,
      fitFunc = x$fitFunc,
      plotlyFunc = x$plotlyFunc
    )
  })

  # Convert the list to a named list for distributionInfoList
  distributionInfoList <- setNames(parsed_list, names(parsed_list))
  # Load the fit and plotly functions for each distribution
  distributionInfoList <- load_functions(distributionInfoList)
  return(distributionInfoList)
}

load_functions <- function(distributionInfoList) {
  for (name in names(distributionInfoList)) {
    fitFuncName <- distributionInfoList[[name]]$fitFunc
    if (exists(fitFuncName)) {
      distributionInfoList[[name]]$fitFunc <- get(fitFuncName)
    } else {
      distributionInfoList[[name]]$fitFunc <- NULL
      print(paste0("[WARNING] fit function ", fitFuncName, " does not exist"))
    }
    plotlyFuncName <- distributionInfoList[[name]]$plotlyFunc
    if (exists(plotlyFuncName)) {
      distributionInfoList[[name]]$plotlyFunc <- get(plotlyFuncName)
    } else {
      distributionInfoList[[name]]$plotlyFunc <- NULL
      print(paste0("[WARNING] plotly function ", plotlyFuncName, " does not exist"))
    }
  }
  return(distributionInfoList)
}

writeDistributionInfo <- function(distributionInfoList, file_path) {
  data_to_save <- lapply(distributionInfoList, as.list)
  write_yaml(data_to_save, file_path)
}