#' Estimation method
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha value for test
#' @return list with data, estimations, values for s, fg and ro
estimate25 <- function(x, n, alpha) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))

  y[1] <- (x[1] + 1) / 3
  s[1] = 1
  ro[1] = 1
  k <- 1

  for (i in 2:length(x)) {
    if (k + 2 * n < i) {
      for (j in seq(i - n, k + n, by = -n)) {
        n1 <- sum(x[k:j])
        n2 <- sum(x[(j + 1):i])
        l1 <- j - k + 1
        l2 <- i - j

        alphap <- alpha / (log(i - k + 1) - log(n))

        x1 = sum(x[k:j])
        x2 = sum(x[(j + 1):i])
        x1n = j - k + 1 - x1
        x2n <- i - j - x2

        p <- computeoddsc(j - k + 1, i - j, x1, x2, 4)

        if (p < alphap) {
          k <- j
          break
        }
      }
    }
    y[i] <- (sum(x[k:i]) + 1) / (i - k + 3)
    s[i] <- i - k + 1
    ro[i] <- (s[i] - 1) / s[i - 1]
  }

  return(list(
    data = x,
    estimate = y,
    s = s,
    ro = ro
  ))
}

#' Function that calls to estimate25 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp25 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.numeric(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate25(x, y, alpha), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=25, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(25, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
