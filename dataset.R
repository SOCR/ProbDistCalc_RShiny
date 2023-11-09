
dataset <- iris

# Imputation of categorical variables using Mode
getmode <- function(v) {
  v <- v[nchar(as.character(v)) > 0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Imputation
medianModeImputation <- function(df) {
  for (cols in colnames(df)) {
    if (cols %in% names(df[, sapply(df, is.numeric)])) { ## Numeric variables first, then the Categorical
      df <- df %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm = TRUE)))
    } else {
      df <- df %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols) == "", getmode(!!rlang::sym(cols))))
    }
  }
  return(df)
}

fitCurrent <- NULL
# print(dataset)
dataset <- medianModeImputation(dataset)

# named list of features
namedListOfFeatures <- function() {
  namedList <- as.list(colnames(dataset))
  names(namedList) <- colnames(dataset)
  return(namedList)
}
