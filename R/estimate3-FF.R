#' Function that estimates the probabilities with a constant forgeting
#' factor ro
#' @param x stream to analyze
#' @param ro forgetting factor
#' @return list with data, estimations, values for s, fg and ro
estimate3 <- function(x, ro) {
  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  rov <- rep(ro, length(x))
  fg <- rep(0, length(x))
  r <- rep(0, length(x))

  r[1] = x[1]
  s[1] = 1
  fg[1] = 0
  for (i in 2:length(x)) {
    r[i] <- r[i - 1] * ro + x[i]
    s[i] <-  s[i - 1] * ro + 1
    fg[i] <- s[i - 1] * (1 - ro)
  }
  y <- (r + 1) / (s + 2)

  return(list(data=x, estimate=y, s=s, fg=fg, ro=ro))
}

#' Function that calls to estimate3 for a set of parameters
#' @param x stream to analyze
#' @param param parameters for the method of estimation
#' @param rp real value of p parameter
#' @param iteration iteration related to this experiment
sexp3 <- function(x, param, rp, iteration) {
  ros <- as.numeric(param[2:length(param)])

  # prepare the cluster
  nCores <- parallel::detectCores()-2
  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, utils::ls.str(.GlobalEnv))

  h <- parallel::parSapply(cl, ros, function(y) {
    elapsedTime <- microbenchmark::microbenchmark(z <- estimate3(x, y), times=1)
    # plot(z[[1]], type = "l")
    l <- kl(z$estimate, rp)

    # saves the results in a file
    saveResult(method=3, iteration=iteration, args=c(ro=y),
               results=z, time=elapsedTime$time, realp=rp, kl=l)

    return(l)

  })

  # stop cluster
  parallel::stopCluster(cl)

  met <- rep(3, length(ros))
  arg <- ros
  return(list(h, met, arg))
}
