#' Estimation method named BAF
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha value for test
#' @return list with data, estimations, values for s, fg and ro
estimate34 <- function(x, n, alpha) {
  y <- rep(1, length(x))
  s   <- rep(1, length(x))
  ro <- rep(1, length(x))
  fg <- rep(1, length(x))
  r <- rep(1, length(x))

  param <- seq(0.0, 1.0, length = n + 1)
  prob <- rep(1 / (n + 1), n + 1)

  for (i in 1:length(x)) {
    for (j in 0:n) {
      prob[j + 1] <- prob[j + 1] * (1 - alpha) + alpha * (1 / (n + 1))
    }
    sumv <- 0

    if (x[i] == 1) {
      for (j in 0:n) {
        prob[j + 1] <- prob[j + 1] * param[j + 1]
        sumv <- sumv + prob[j + 1]
      }
    }
    else {
      for (j in 0:n) {
        prob[j + 1] <- prob[j + 1] * (1 - param[j + 1])
        sumv <- sumv + prob[j + 1]
      }
    }
    prob <- prob / sumv
    y[i] = sum(prob * param)

  }

  return(list(
    data = x,
    estimate = y,
    s = s,
    fg = fg,
    ro = ro
  ))
}

#' Function that calls to estimate34 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp34 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.numeric(param[4])
  cat("window sizes - initial value: ", n1, " final value: ", n2, "alpha: ", alpha, "\n")

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate34(x, y, alpha), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=34, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(34, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
