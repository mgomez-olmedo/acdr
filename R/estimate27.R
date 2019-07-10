#' Estimation method
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha value for test
#' @return list with data, estimations, values for s, fg and ro
estimate27 <- function(x, n, alpha) {
  y <- vector(1, length(x))
  s   <- vector(1, length(x))
  ro <- rep(1, length(x))
  fg <- vector(1, length(x))
  r <- vector(1, length(x))

  r[1] = x[1]
  s[1] = 1
  fg[1] = 0
  ro[1] = 1
  y[1] <- (r[1] + 1) / (s[1] + 2.0)

  for (i in 2:length(x)) {
    if (i <= 2 * n) {
      r[i] <- x[i] + r[i - 1]
      s[i] <- s[i - 1] + 1

    }
    else{
      odd <- computeoddsc(s[i - n], n, r[i - n], sum(x[(i - n + 1):(i)]), 4)
      if (odd < alpha) {
        prob <- (odd / (1 + odd))
      }
      else {
        prob = 1
      }

      if (prob > (s[i - 1] - n + 1) / (s[i - n])) {
        prob = (s[i - 1] - n + 1) / (s[i - n])
      }

      if (prob == 1) {
        r[i] = (prob) * (sum(r[(i - 2 * n + 1):(i - n)])) / n + sum(x[(i - n + 1):(i)])
        s[i] =  (prob) * (sum(s[(i - 2 * n + 1):(i - n)])) / n + n
      }
      else {
        r[i] = (prob) * r[i - n] + sum(x[(i - n + 1):(i)])
        s[i] =  (prob) * s[i - n] + n
      }
    }

    y[i] <- (r[i] + 1) / (s[i] + 2.0)
  }

  return(list(
    data = x,
    estimate = y,
    s = s,
    fg = fg,
    ro = ro
  ))
}

#' Function that calls to estimate27 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp27 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.numeric(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate27(x, y, alpha), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=27, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(27, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
