#' Function that estimates the probabilities with a  forgetting factor
#' which is computed by a Bayesian BDEu score of the probability of change
#' in the last n cases (dividing these cases in two equal parts)
#' @param x stream to analyze
#' @param n size of window with last cases to consider for estimation
#' @param alpha value to consider for tests
#' @return list with data, estimations, values for s, fg and rho
estimate5 <- function(x, n, alpha) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <- rep(1, length(x))
  fg <- rep(0, length(x))
  r <- rep(0, length(x))

  r[1] = x[1]
  s[1] = 1
  fg[1] = 0
  ro[1] = 1
  y[1] <- (r[1] + 1) / (s[1] + 2.0)

  for (i in 2:length(x)) {
    if (i < n) {
      ro[i] = 1.0
    }
    else{
      n1 <- n %/% 2
      n2 <- n - n1
      odd <-
        computeoddsc(n1, n2, sum(x[(i - n + 1):(i - n2)]), sum(x[(i - n2 + 1):(i)]), 4)
      if (odd > alpha) {
        ro[i] <- 1.0
      }
      else {
        ro[i] <- odd ^ {
          1 / 20
        }
      }
    }
    r[i] <- r[i - 1] * ro[i] + x[i]
    s[i] <-  s[i - 1] * ro[i] + 1
    fg[i] = s[i - 1] * (1 - ro[i])
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

#' Function that calls to estimate5 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp5 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  alpha <- as.double(param[4])
  cat("n1: ", n1, " n2: ", n2, " alpha: ", alpha, "\n")

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate5(x, y, alpha), times=1)

    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=5, iteration=iteration, args=c(n=y, alpha=alpha),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)
  met <- rep(5, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}
