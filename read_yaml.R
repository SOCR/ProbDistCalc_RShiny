library(yaml)
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
load_distribution_info <- function(file_path) {
  # Read the YAML file
  yaml_data <- read_yaml(file_path)

  # Convert the list of lists back to distributionInfoList
  parsed_list <- lapply(yaml_data, function(x) {
    distributionInfoClass(
      id = x$id,
      name = x$name,
      inputNames = x$inputNames,
      labels = x$labels,
      defaultValues = x$defaultValues,
      hasImplementation = x$hasImplementation,
      isWithSD = x$isWithSD,
      fitFunc = x$fitFunc
    )
  })

  # Convert the list to a named list for distributionInfoList
  distributionInfoList <- setNames(parsed_list, names(parsed_list))

  return(distributionInfoList)
}

# Usage example:
parsed_distribution_info <- load_distribution_info("distribution_info.yaml")

print(parsed_distribution_info)