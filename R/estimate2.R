#' Function that estimates probabilities from a string x
#' It returns a list with the estimations, the sample sizes, the forgotten
#' samples, and the forgetting coefficients
#' It makes statistical tests (goodness of fit) of the n1 last samples with
#' respect to the estimations done on step i-n1. If test is meaningfull, then
#' it multiplies the past samples by a coefficient rho
#' m is a value such that the p-value is squared to m
#' @param x stream to analyze
#' @param n1 number of recent samples to consider
#' @param m p-value will be squared to m
estimate2 <- function(x, n1, m) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(1, length(x))
  fg <- rep(0, length(x))
  r <- rep(0, length(x))
  past <- rep(0, length(x))
  r[1] = x[1]
  ro[1] = 1
  s[1] = 1
  y[1] = (r[1] + 1) / (s[1] + 2.0)
  fg[1] = 0
  past[1] <- 0

  for (i in 2:length(x)) {
    if (i > 2 * n1) {
      pvalue <- stats::binom.test(sum(x[(i - n1 + 1):i]), n1, y[i - n1])$p.value
      if (pvalue > 0.2) {
        ro[i] <- 1
      }
      else {
        ro[i] <- (pvalue / 0.2) ^ {
          1 / m
        }
      }
    }
    else {
      ro[i] <- 1
    }
    if (i <= n1 + 1) {
      y[i] <- (sum(x[1:i]) + 1) / (i + 2.0)

      fg[i] <- 0
      s[i] <-  s[i - 1] + 1
      past[i] <- 0
    }
    else{
      past[i - n1] <- (past[i - n1 - 1] + x[i - n1]) * ro[i]
      s[i - n1] <- (s[i - n1 - 1] + 1) * ro[i]
      y[i] <-
        (sum(x[(i - n1 + 1):i]) + past[i - n1] + 1) / (s[i - n1] + n1 +
                                                         2.0)
    }
  }

  # return result
  return(list(
    data = x,
    estimate = y,
    s = s,
    fg = fg,
    ro = ro
  ))
}

#' Function that calls to estimate2 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp2 <- function(x, param, rp, iteration) {
  n1 <- as.integer(param[2])
  n2 <- as.integer(param[3])
  m <- as.numeric(param[4])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  # performs the experiments
  h <- parallel::parSapply(cl, n1:n2, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate2(x, y, m), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=2, iteration=iteration, args=c(n1=y, m=m),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)
  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(2, n2 - n1 + 1)
  arg <- n1:n2

  return(list(h, met, arg))
}

