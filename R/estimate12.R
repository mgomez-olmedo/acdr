#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, and the
#' forgotten samples
#' It contains a window of active values.
#' It is similar to estimate7, but now it carries out a chisquared test
#' @param x stream to analyze
#' @param n size of active window
#' @param alpha1 parameter for test
#' @param alpha2 parameter for test
#' @return list with data, estimations and values of s and ro
estimate12 <- function(x, n, alpha1, alpha2) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))
  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    if (i - k >= 2 * n) {
      l <- 0

      j1 = i - n
      j2 = i - n + 1

      x1 = sum(x[k:j1])
      x2 = sum(x[j2:i])
      x1n = j1 - k + 1 - x1
      x2n <- i - j2 + 1 - x2

      total <- i - k + 1

      e11 <- (x1 + x1n) * (x1 + x2) / total
      e12 <-  (x2 + x2n) * (x1 + x2) / total
      e21 <-  (x1 + x1n) * (x1n + x2n) / total
      e22 <-  (x2 + x2n) * (x1n + x2n) / total

      tab <- array(c(x1, x1n, x2, x2n), dim = c(2, 2))

      if ((e11 >= 5) && (e12 >= 5) && (e21 >= 5) && (e22 >= 5)) {
        p <-   stats::chisq.test(tab)$p.value
      }
      else
      {
        p <- stats::fisher.test(tab)$p.value
      }

      if (p < alpha2) {
        l <- (j2 - k)
        k <- j2
      }
      else if (p < alpha1)
      {
        l <- floor((1 - (p - alpha2) / (alpha1 - alpha2)) * (j2 - k))
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

#' Function that calls to estimate12 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp12 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha1 <- as.numeric(param[4])
  alpha2 <- as.numeric(param[5])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate12(x, y, alpha1, alpha2), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=12, iteration=iteration, args=c(n=y, alpha1=alpha1, alpha2=alpha2),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(12, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
