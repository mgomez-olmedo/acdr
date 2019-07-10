#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the forgotten samples
#' forg is a vector of rhos. It considers all the rhos and selects
#' the rho with maximum likelihood in each case.
#' @param x stream to analyze
#' @param forg vector of values for rho parameter
#' @param l length to consider
#' @return list with data, estimations and values of s and ro
estimate10 <- function(x, forg, l) {
  count <- rep(0, l)
  sample <- rep(0, l)
  logd <-  rep(0, l)

  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))

  count = rep(x[1], l)
  sample = rep(1, l)

  y[1] <- (x[1] + 1) / 3
  s[1] = 1
  ro[1] = 1

  for (i in 2:length(x)) {
    for (j in 1:l) {
      count[j] <- x[i] + forg[j] * count[j]
      sample[j] <- 1 + forg[j] * sample[j]
      #     print(count[j])
      if (y[1] == 1) {
        logd[j] <- logd[j] + log((count[j] + 1) / (sample[j] + 2))
      }
      else {
        logd[j] <-
          logd[j]  + log((sample[j] - count[j] + 1) / (sample[j] + 2))
      }
    }
    logd = logd - max(logd)
    j <- which.max(logd)

    y[i] <- (count[j] + 1) / (sample[j] + 2)
    s[i] <- (sample[j])
    ro[i] =  forg[j]
  }

  return(list(
    data = x,
    estimate = y,
    s = s,
    ro = ro
  ))
}

#' Function that calls to estimate10 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp10 <- function(x, param, rp, iteration) {
  l <- length(param) - 1

  forg <- as.double(param[2:(l + 1)])

  elapsedTime <- microbenchmark::microbenchmark(z <- estimate10(x, forg, l), times=1)

  # plot(z[[1]], type = "l")
  l <- kl(z$estimate, rp)
  par <- list(forg)

  # saves the results in a file
  saveResult(method=10, iteration=iteration, args=c(forg=forg),
             results=z, time=elapsedTime$time, realp=rp, kl=l)

  return(list(l, 10, par))
}
