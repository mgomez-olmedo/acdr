#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the forgotten
#' samples
#' It contains a window of active values with fixex size n
#' @param x stream to analyze
#' @param n size of active window
#' @return list with data, estimations, values of s and ro
estimate4 <- function(x, n) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    if (i - k >= n) {
      l <- 1
      k <- k + 1
    }

    y[i] <- (sum(x[k:i]) + 1) / (i - k + 1 + 2)
    s[i] <-  i - k + 1
    ro[i] <- l
  }

  return(list(data=x, estimate=y, s=s, ro=ro))
}

#' Function that calls to estimate4 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp4 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate4(x, y), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=4, iteration=iteration, args=c(n=y),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(4, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}

