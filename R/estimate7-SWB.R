#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the
#' forgotten samples
#' It contains a window of active values.
#' It is similar to estimate1 and estimate5, but now the window of active
#' values is divided in two
#' parts if its length is greater than n: one with the last n values and other
#' with the rest of values
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha value for tests
#' @return list with data, estimations and values of s and ro
estimate7 <- function(x, n, alpha) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    if (i - k >= n) {
      l <- 0

      j1 = i - n
      j2 = i - n + 1

      x1 = sum(x[k:j1])
      x2 = sum(x[j2:i])

      odd <- computeoddsc(i - k - n + 1, n, x1, x2, 4)

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

#' Function that calls to estimate7 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp7 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.double(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate7(x, y, alpha), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=7, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(7, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}

