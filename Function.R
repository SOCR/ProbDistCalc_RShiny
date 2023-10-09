distribution_function <- function (distType, outcome){
  # Use 'which' to find the index of the target distribution
  index_target_distribution <- which(distributions == distType)
  print(index_target_distribution)
  
  #Beta Distribution
  if (index_target_distribution == 5){
    # Transform the data to fit within [0, 1] range (assuming min-max scaling)
    scaled_data <- (dataset[, outcome] - min(dataset[, outcome])) / (max(dataset[, outcome]) - min(dataset[, outcome]))
    # Fit a beta distribution to the scaled data
    fitDistModel <- fitdist(scaled_data, "beta", method = "mme")
    # Generate random samples from the fitted beta distribution
    modelFit <- rbeta(10000, shape1 = fitDistModel$estimate[[1]], shape2 = fitDistModel$estimate[[2]])
    # Transform the samples back to the original range
    original_modelFit <- modelFit * (max(dataset[,outcome]) - min(dataset[,outcome])) + min(dataset[, outcome])
    # Create density estimate for the samples
    fit <- density(original_modelFit, bw = 1)
    # Create the plot
    my_plot <- create_plot(outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #Exponential Distribution
  else if (index_target_distribution == 23){
    fitDistModel <- fitdist(dataset[,  outcome], "exp")
    modelFit <- rexp(10000, rate = fitDistModel$estimate[[1]])
    fit <- density(modelFit, bw= 1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #Gamma Distribution
  else if (index_target_distribution == 27){
    fitDistModel  <- fitdist(dataset[, outcome], "gamma")
    modelFit <- rgamma(10000, shape=fitDistModel$estimate[[1]],
                       rate=fitDistModel$estimate[[2]])
    fit <- density(modelFit, bw=1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
    
  }
  #Geometric Distribution (geometric is only available for data with non negative integers.)
  else if (index_target_distribution == 30){
    rounded_data <- round(dataset[, outcome])
    fitDistModel <- fitdist(rounded_data, "geom")
    modelFit <- rgeom(10000, prob = fitDistModel$estimate[[1]])
    fit <- density(modelFit, bw = 1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #logistic Distribution
  else if (index_target_distribution == 44){
    fitDistModel <- fitdist(dataset[, outcome], "logis")
    modelFit <- rlogis(10000, location = fitDistModel$estimate[[1]], scale = fitDistModel$estimate[[2]])
    fit <- density(modelFit, bw = 1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
    
  }
  #log-normal Distribution
  else if (index_target_distribution == 46){
    fitDistModel <- fitdist(dataset[, outcome], "lnorm")
    modelFit <- rlnorm(10000, meanlog = fitDistModel$estimate[[1]], sdlog = fitDistModel$estimate[[2]])
    fit <- density(modelFit, bw = 1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #Negative-Binomial Distribution (negative binomial is only available for data with non negative integers.)
  else if (index_target_distribution == 54){
    rounded_data <- round(dataset[, outcome])
    fitDistModel <- fitdist(rounded_data, "nbinom")
    print(fitDistModel$estimate)
    modelFit <- rnbinom(10000, size = fitDistModel$estimate[[1]], mu = fitDistModel$estimate[[2]])
    fit <- density(modelFit, bw=1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #Normal Distribution
  else if (index_target_distribution == 57){
    fitDistModel  <- fitdist(dataset[, outcome], "norm")
    modelFit <- rnorm(10000, mean = fitDistModel$estimate[[1]], sd = fitDistModel$estimate[[2]])
    fit <- density(modelFit, bw=1)
    my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    return(my_plot)
  }
  #Poisson Distribution (poisson is only available for data with non negative integers.)
  else if (index_target_distribution == 61){
    # #Transform the data to integers.
    # 
    # Round the outcome variable
    rounded_data <- round(dataset[, outcome])
    
    # Fit a Poisson distribution
    fitDistModel <- fitdist(rounded_data, "pois", discrete = TRUE)
    
    # Calculate the PMF values for each possible outcome
    pmf_values <- dpois(0:max(rounded_data), lambda = fitDistModel$estimate[[1]])
    
    # Create a bar plot for the PMF
    my_plot <- # Create a Plotly bar plot for the PMF
      plot_ly(x = 0:max(rounded_data), y = pmf_values, type = "bar") %>%
      layout(xaxis = list(title = "Outcome"),
             yaxis = list(title = "Probability"),
             title = "Poisson Distribution PMF")
    
    # rounded_data <- round(dataset[, outcome])
    # fitDistModel <- fitdist(rounded_data, "pois", discrete = TRUE)
    # modelFit <- rpois(10000, lambda = fitDistModel$estimate[[1]])
    # fit <- density(modelFit, bw = 1)
    # my_plot <- create_plot( outcome, dataset, fit, fitDistModel, distType)
    
    return(my_plot)
  }
  
}

create_plot <- function(outcome, dataset, fit, fitDistModel, distType) {
  my_plot <- plot_ly(x = ~unlist(dataset[,  outcome]), type = "histogram",
                     histnorm = "probability", name = "Data Histogram") %>%
    add_trace(x = ~fit$x, y = ~fit$y, type = "scatter",
              mode = "lines", opacity = 0.1, fill = "tozeroy",
              name = modelString(fitDistModel, distType))  %>%
    layout(title = 'Data Histogram & Probability Distribution Model',
           xaxis = list(title = "Height"), yaxis = list(title = "relative frequency/density"),
           legend = list(orientation = 'h', y = -0.3))
  
  return(my_plot)
}

modelString <- function (fitDistModel, distType){
  num <- length(fitDistModel$estimate)
  tempnum <- 0
  result <- distType
  if (num == 0) {
    return (result)
  }
  else{
    result <- paste0(result, " (")
    while (num != 0){
      result <- paste0(result, names(fitDistModel$estimate)[tempnum + 1], "=",
                       round(fitDistModel$estimate[[tempnum + 1]], 2))
      num <- num - 1
      tempnum <- tempnum + 1
      if (num != 0){
        result <- paste0(result, " ")
      }
    }
    result <- paste0(result, ")")
    return (result)
  }
}