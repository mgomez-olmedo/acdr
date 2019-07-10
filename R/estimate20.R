#' Estimation method
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha value for test
#' @return list with data, estimations, values for s, fg and ro
estimate20 <- function(x, n, alpha) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    if (i - k >= 2 * n - 1) {
      test <- 1
      l <- 0

      while (test == 1) {
        j1 = k + n - 1
        j2 = i - n + 1

        x1 = sum(x[k:j1])
        x2 = sum(x[j2:i])

        odd <- computepvalue(x1, x2, n - x1, n - x2)

        if (odd < alpha) {
          k <- k + n
          l <- l + n
          if (i - k < 2 * n - 1) {
            test <- 0
          }
        }
        else {
          test <- 0
        }
      }
    }

    y[i] <- (sum(x[k:i]) + 1) / (i - k + 1 + 2)
    s[i] <-  i - k + 1
    ro[i] <- l
  }

  return(list(
    data = x,
    estimate = y,
    s = s,
    ro = ro
  ))
}

#' Function that calls to estimate20 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp20 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha1 <- as.numeric(param[4])
  alpha2 <- as.numeric(param[5])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate20(x, y, alpha1), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=20, iteration=iteration, args=c(n=y, alpha1=alpha1, alpha2=alpha2),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(20, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
