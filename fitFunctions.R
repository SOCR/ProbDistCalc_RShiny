
 fitAndersonDarling <- function(dataset) {
     fitDistModel <- fitdist(dataset, "andersonDarling")
     return(fitDistModel)
 }


fitArcSine <- function(dataset) {
  dataset_scaled <- (dataset - min(dataset)) / (max(dataset) - min(dataset))
  dcustom <- function(x, a, b) {
    ifelse(x > a & x < b, 1 / (pi * sqrt((x - a) * (b - x))), 0)
  }
  
  pcustom <- function(q, a, b) {
    if (q > a & q < b) {
      return(asin(sqrt((q - a) / (b - a))) / (pi / 2))
    } else if (q <= a) {
      return(0)
    } else {
      return(1)
    }
  }
  
  qcustom <- function(p, a, b) {
    if (p >= 0 & p <= 1) {
      return(a + (b - a) * sin((pi / 2) * p)^2)
    } else if (p < 0) {
      return(a)
    } else {
      return(b)
    }
  }
  fitDistModel <- fitdist( dataset_scaled, "custom", method = "mle", start = list(a = 0, b = 1)) 
   return(fitDistModel)
 }

fitBenford <- function(dataset) {
  single_cell_table <- data.frame("estimate" = 1)
  row.names(single_cell_table) <- "Benfn"
  
  return(single_cell_table)
}

fitBernoulli <- function(dataset) {
  
  p_hat <- mean(dataset)  
  return(list(estimate = p_hat))
}



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


library(extraDistr)
library(bbmle)
 # Round to nearest integer

fitBetaBinomial <- function(dataset) {
  dataset <- round(dataset) 
  size <- ceiling(max(dataset))
  
  # Define the negative log-likelihood function for Beta-Binomial
  neg_log_likelihood <- function(alpha, beta) {
    if (alpha <= 0 || beta <= 0) return(Inf)
    -sum(dbbinom(dataset, size = size, alpha = alpha, beta = beta, log = TRUE))
  }
  
  # Fit the model using MLE
  fit <- mle2(
    neg_log_likelihood,
    start = list(alpha = 1, beta = 1),
    method = "L-BFGS-B",
    lower = c(0.0001, 0.0001),
    control = list(maxit = 1000)
  )
  fit_df <- data.frame(
    estimate = c( size,coef(fit)["alpha"], coef(fit)["beta"]),
    row.names = c( "n", "alpha", "beta")
  )
  
  return(fit_df)
}






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

library(minpack.lm)

fitCircle <- function(dataset) {
  # Define residual function to minimize
  circleResiduals <- function(params) {
    r <- params[1]  # radius of the circle
    c <- params[2]  # center of the circle
    abs(dataset - c) - r  # residuals: difference from each point to radius centered at c
  }
  
  # Provide initial guesses for radius and center
  start_center <- mean(dataset)  # initial guess for center
  start_radius <- max(abs(dataset - start_center))  # initial guess for radius
  
  # Fit radius and center using nonlinear least squares
  fitResult <- nls.lm(par = c(start_radius, start_center), fn = circleResiduals, control = list(maxiter = 100))
  
  # Extract the fitted radius and center
  fitted_radius <- fitResult$par[1]
  fitted_center <- fitResult$par[2]
  
       
       # Return a data frame with one row and one column containing the fitted radius
  fit_df <- data.frame(
    estimate = c( fitted_radius, fitted_center),
    row.names = c( "radius", "center")
  )
       return(fit_df)
       
     
   
 }

# Not tested
fitContinuousUniform <- function(dataset) {
    fitDistModel <- fitdist(dataset, "unif")
    return(fitDistModel)
}

coupon_likelihood <- function(params, data) {
  N <- params[1]  # Total population size (number of distinct items)
  k <- params[2]  # Number of distinct items needed
  
  # Calculate expected trials based on harmonic sum approximation
  expected_trials <- N * sum(1 / (N - (0:(k - 1))))
  
  # Round data to integer values for Poisson
  rounded_data <- round(data)
  
  # Calculate the negative log-likelihood using the rounded data
  -sum(dpois(rounded_data, lambda = expected_trials, log = TRUE))
}
# Not tested
coupon_likelihood <- function(params, data) {
  N <- params[1]  # Total population size (number of distinct items)
  k <- params[2]  # Number of distinct items needed
  
  # Calculate expected trials based on harmonic sum approximation
  expected_trials <- N * sum(1 / (N - (0:(k - 1))))
  
  # Round data to integer values for Poisson
  rounded_data <- round(data)
  
  # Calculate the negative log-likelihood using the rounded data
  -sum(dpois(rounded_data, lambda = expected_trials, log = TRUE))
}
fitCoupon <- function(dataset) {
  # Initial guesses for N and k
  start_params <- c(N = max(dataset), k = round(mean(dataset) / 2))
  
  # Perform optimization with integer constraint on k
  fit_result <- optim(
    par = start_params,
    fn = coupon_likelihood,
    data = dataset,
    method = "L-BFGS-B",
    lower = c(1, 1),
    upper = c(Inf, Inf)
  )
  
  # Extract fitted parameters, rounding k to the nearest integer
  N_estimate <- round(fit_result$par[1])
  k_estimate <- round(fit_result$par[2])  # Ensure integer k
  
  # Return the results as a data frame
  fit_df <- data.frame(
    estimate = c(N_estimate, k_estimate),
    row.names = c("Population Size", "Number of distinct values needed")
  )
  
  return(fit_df)
}

# Not tested
fitDie <- function(dataset) {
  getDensity_Die <- function(x) {
    k <- round(x)
    if (n_Die == 0) {
      # FAIR Die
      if (k < 1 || k > 6) {
        return(0)
      } else {
        return(1/6)
      }
    } else if (n_Die == 1) {
      # FLAT16 Die
      if (k < 1 || k > 6) {
        return(0)
      } else if (k == 1 || k == 6) {
        return(1/4)
      } else {
        return(1/8)
      }
    } else if (n_Die == 2) {
      # FLAT25 Die
      if (k < 1 || k > 6) {
        return(0)
      } else if (k == 2 || k == 5) {
        return(1/4)
      } else {
        return(1/8)
      }
    } else if (n_Die == 3) {
      # FLAT34 Die
      if (k < 1 || k > 6) {
        return(0)
      } else if (k == 3 || k == 4) {
        return(1/4)
      } else {
        return(1/8)
      }
    } else if (n_Die == 4) {
      # LEFT Die
      if (k < 1 || k > 6) {
        return(0)
      } else if (k == 1) {
        return(1/21)
      } else if (k == 2) {
        return(2/21)
      } else if (k == 3) {
        return(3/21)
      } else if (k == 4) {
        return(4/21)
      } else if (k == 5) {
        return(5/21)
      } else {
        return(6/21)
      }
    } else if (n_Die == 5) {
      # RIGHT Die
      if (k < 1 || k > 6) {
        return(0)
      } else if (k == 1) {
        return(6/21)
      } else if (k == 2) {
        return(5/21)
      } else if (k == 3) {
        return(4/21)
      } else if (k == 4) {
        return(3/21)
      } else if (k == 5) {
        return(2/21)
      } else {
        return(1/21)
      }
    }
  }
  best_die_type <- NULL
  lowest_residual <- Inf
  residuals_list <- list()  
  
  
  for (die_type in 0:5) {
    n_Die <<- die_type  
    expected_density <- sapply(dataset, getDensity_Die)
    residuals <- (1 / length(dataset) - expected_density)^2
    residual_sum <- sum(residuals)
    residuals_list[[paste0("Die_Type_", die_type)]] <- residual_sum
    
    if (residual_sum < lowest_residual) {
      lowest_residual <- residual_sum
      best_die_type <- die_type
    }
  }
  fit_df <- data.frame(
    estimate = best_die_type,
    row.names = 'n (0 <= n <= 5)'
  )
    return(fit_df)
}

# Not tested
fitDiscreteArcSine <- function(dataset) {
    dataset <- round(dataset)
    darcsine <- function(x, a, b) {
      if (a >= b) stop("Invalid range: a must be less than b")
      ifelse(x >= a & x <= b, 1 / (pi * sqrt((x - a) * (b - x))), 0)
    }
    
    # CDF of the arcsine distribution
    parcsine <- function(q, a, b) {
      if (a >= b) stop("Invalid range: a must be less than b")
      ifelse(
        q < a, 0,
        ifelse(
          q > b, 1,
          (2 / pi) * asin(sqrt((q - a) / (b - a)))
        )
      )
    }
    
    # Quantile function (inverse CDF) of the arcsine distribution
    qarcsine <- function(p, a, b) {
      if (a >= b) stop("Invalid range: a must be less than b")
      if (p < 0 | p > 1) stop("Probability p must be in [0, 1]")
      a + (b - a) * sin((p * pi) / 2)^2
    }
    
    fitDistModel <- fitdist( dataset, "arcsine", method = "mle", start = list(a = 1, b = 2)) 
    return(fitDistModel)
}

# Not tested
fitDiscreteUniform <- function(dataset) {
    #rounded_data <- round(dataset)
   fitDistModel <-fitdist(dataset, "unif", method = c("mme"),
            start=NULL, fix.arg=NULL, discrete = TRUE)
    return(fitDistModel)
}

# FIXME: This is not working
# fitErlang <- function(dataset) {
#     fitDistModel <- fitdist(dataset, "erlang")
#     return(fitDistModel)
# }

fitErlang <- function(dataset) {
  mean_data <- mean(dataset)
  var_data <- var(dataset)
  
  # Rough estimate of the shape parameter
  myshape <- round(mean_data^2/var_data)
  
  
  fit_df <- data.frame(
    estimate = c( mean_data/myshape, myshape),
    row.names = c( "scale", "shape")
  )
  return(fit_df)
  
}


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
