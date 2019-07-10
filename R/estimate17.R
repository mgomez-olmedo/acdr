#' Estimation method
#' @param x stream to analyze
#' @param alpha1 value for test
#' @param alpha2 value for test
#' @return list with data, estimations, values for s, fg and ro
estimate17 <- function(x, alpha1, alpha2) {

  y <- rep(0, length(x))
  s   <- rep(0, length(x))
  ro <-  rep(0, length(x))

  l <- 0
  k <- 1

  for (i in 1:length(x)) {
    for (n in 15:35) {
      if (i - k >= 2 * n) {
        l <- 0

        j1 = i - n
        j2 = i - n + 1

        x1 = sum(x[k:j1])
        x2 = sum(x[j2:i])
        x1n = j1 - k + 1 - x1
        x2n <- i - j2 + 1 - x2

        p <- computepvalue(x1, x2, x1n, x2n)

        if (p < alpha2) {
          l <- (j2 - k)
          k <- j2
          break
        }
        else if (p < alpha1)
        {
          d1 <-
            kls((x1 + x2 + 1) / (x1 + x1n + x2 + x2n + 2), (x2 + 1) / (x2 + x2n +
                                                                         2))
          d2 <-
            kls((x2 + 1) / (x2 + x2n + 2), (x1 + x2 + 1) / (x1 + x1n +
                                                              x2 + x2n + 2))

          if (d2 > 0) {
            dr <- 2 * d2 / (d1 + d2)
            l <-
              floor((1 - (p - alpha2) / (alpha1 - alpha2)) * (j2 - k) *
                      dr)
            l <- min(l, (j2 - k))
          }
          else {
            l <- 0
          }
          k <- k + l
          break
        }
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

#' Function that calls to estimate17 for a set of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp real value of p parameter for each sample
#' @param iteration number of iteration
#' @return list with results of execution
sexp17 <- function(x, param, rp, iteration) {
  alpha1 <- as.numeric(param[2])
  alpha2 <- as.numeric(param[3])

  elapsedTime <- microbenchmark::microbenchmark(h <- estimate17(x, alpha1, alpha2), times=1)

  # plot(h[[1]], type = "l")
  l <- kl(h$estimate, rp)

  met <- rep(17, 1)
  arg <-  c(alpha1, alpha2)

  # saves the results in a file
  saveResult(method=17, iteration=iteration, args=c(alpha1=alpha1, alpha2=alpha2),
             results=h, time=elapsedTime$time, realp=rp, kl=l)

  return(list(l, 17, arg))
}
