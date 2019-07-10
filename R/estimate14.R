#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the
#' forgotten samples
#' It contains a window of active values.
#' It is similar to estimate1 and estimate5, but now the window of active
#' values is divided in two
#' parts if its length is greater than n: one with the last n values and
#' other with the rest of values
#' @param x stream to analyze
#' @param n size of active window
#' @param delta parameter for test
#' @return list with data, estimations and values of s and ro
estimate14 <- function(x, n, delta) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  deltap <- delta
  k <- 1

  for (i in 1:length(x)) {
    if ((i - k) > n + 3) {
      j <- i - n
      n1 <- sum(x[k:j])
      n2 <- sum(x[(j + 1):i])
      l1 <- j - k + 1
      l2 <- i - j

      m <- 1 / (1 / l1 + 1 / l2)
      cut <- sqrt(1 / (2 * m) * ((n1 + n2) / (i - k + 1)) *
                    ((i - k + 1 - n1 - n2) / (i - k + 1)) *
                    log(2 / deltap)) + 2 / (3 * m) *
        log(2 / deltap)
      if (abs(n1 / l1 - n2 / l2) > cut) {
        l <- j - k + 1
        k <- k + l
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

#' Function that calls to estimate14 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp14 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.numeric(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate14(x, y, alpha), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=14, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(14, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
