#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the
#' forgotten samples
#' It contains a window of active values.
#' It is similar to estimate1, but now the window of active
#' values is divided by two, if the total size is greater
#' than n
#' @param x stream of data to analyze
#' @param n size of active window
#' @param alpha for tests
#' @return list of data, estimations, values of s and ro
estimate6 <- function(x, n, alpha) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    if (i - k >= 2 * n - 1) {
      l <- 0

      j1 = k + (i - k) %/% 2
      j2 = j1 + 1

      x1 = sum(x[k:j1])
      x2 = sum(x[j2:i])

      odd <- computeoddsc(j1 - k + 1, i - j2 + 1, x1, x2, 4)

      if (odd < alpha) {
        l <- j1 - k + 1
        k <- j2
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

#' Function that calls to estimate6 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp6 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.double(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate6(x, y, alpha), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=6, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(6, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
