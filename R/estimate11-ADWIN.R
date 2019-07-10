#' Base estimation method termed ADWIN
#' @param x stream to analyze
#' @param delta parameter
#' @return list with data, estimations and values of s and ro
estimate11 <- function(x, delta) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))

  y[1] <- (x[1] + 1) / 3
  s[1] = 1
  ro[1] = 1
  k <- 1

  for (i in 2:length(x)) {
    fin <- FALSE
    while (!fin) {
      fin <- TRUE
      for (j in k:(i - 1)) {
        n1 <- sum(x[k:j])
        n2 <- sum(x[(j + 1):i])
        l1 <- j - k + 1
        l2 <- i - j

        m <- 1 / (1 / l1 + 1 / l2)
        deltap <- delta / log(i - k + 1)
        cut <- sqrt(1 / (2 * m) * ((n1 + n2) / (i - k + 1)) *
                      ((i - k + 1 - n1 - n2) / (i - k + 1)) *
                      log(2 / deltap)) + 2 / (3 * m) *
          log(2 / deltap)
        if (abs(n1 / l1 - n2 / l2) > cut) {
          fin <- FALSE
          k <- k + 1
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

#' Function that calls to estimate11 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp11 <- function(x, param, rp, iteration) {
  l <- length(param)
  delta <- as.double(param[2:l])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, 1:(l - 1), function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate11(x, delta[y]), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=11, iteration=iteration, args=c(delta=delta[y]),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(11, l - 1)
  arg <- delta

  return(list(h, met, arg))
}

