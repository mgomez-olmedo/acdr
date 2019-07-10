library(readr)

#' estimates the concept drift with email data
estimateEmail <- function() {
  # read data
  email_data <- readr::read_csv("./data/email_data.csv",
                                readr::cols(.default = readr::col_integer()),
                                col_names = TRUE)

  # Compute a 0-1 matrix from email_data usenet
  email <- vector(mode = "integer", length = 1500 * 914)
  dim(email) = c(1500, 914)
  for (j in 1:914) {
    for (i in 1:1500) {
      email[i, j] = email_data[[i, j]]
    }
  }

  # Estimate the prior probabilities of vector class (unknown probability
  # of change)
  nat = 913 # Number of variables
  nca = 1500 # Number of cases
  alpha1 = 0.01 # lower value of the interval for probability of change
  alpha2 = 0.02 # upper value of the interval for probability of change
  n = 200 # Number of points for the probability interval
  k = 20 # number of points for the interval of probability of change

  probyes <- vector(mode = "double", length = nca)
  probno <- vector(mode = "double", length = nca)

  x <- email[, nat + 1]
  param <- seq(0.0, 1.0, length = n + 1)
  prob <- rep(1 / ((n + 1) * (k + 1)), (n + 1) * (k + 1))
  prom <- matrix(prob, nrow = k + 1)
  discount <- seq(alpha1, alpha2, length = k + 1)
  delta <- (alpha2 - alpha1) / (k * k)
  changeyes <- rep(0, length(x))

  for (i in 1:k) {
    discount[i + 1] = discount[i] + (2 * i - 1) * delta
  }

  for (i in 1:(length(x))) {
    sumr <- rowSums(prom)
    sumt <- sum(sumr)

    for (l in 0:k) {
      for (j in 0:n) {
        prom[l + 1, j + 1] <-
          (prom[l + 1, j + 1] * (1 - discount[l + 1]) + discount[l + 1] * (1 / (n +
                                                                                  1)) * sumr[l + 1]) / sumt
      }
    }

    probparam <-  colSums(prom)
    probparam <- probparam / sum(probparam)
    sumr <- rowSums(prom)
    sumt <- sum(sumr)
    probdis <- sumr / sumt
    probyes[i] <- sum(probparam * param)
    changeyes[i] <- sum(probdis * discount)

    if (x[i] == 1) {
      for (l in 0:k) {
        prom[l + 1, ] <- prom[l + 1, ] * param
      }
    }
    else {
      for (l in 0:k) {
        prom[l + 1, ] <- prom[l + 1, ] * (1 - param)
      }
    }
  }

  # Estimate the conditional probabilities given the class, Bayesian method,
  # not known probability of change
  probcondyes <- vector(mode = "double", length = nca * nat)
  probcondno <- vector(mode = "double", length = nca * nat)
  condyeschange <- vector(mode = "double", length = nca * nat)
  condnochange <- vector(mode = "double", length = nca * nat)
  dim(probcondyes) = c(nca, nat)
  dim(probcondno) =  c(nca, nat)
  dim(condyeschange) = c(nca, nat)
  dim(condnochange) =  c(nca, nat)
  z <- email[, nat + 1]

  for (ic in (1:nat)) {
    x <- email[, ic]

    for (yes in (0:1)) {
      param <- seq(0.0, 0.1, length = n + 1)
      prob <- rep(1 / ((n + 1) * (k + 1)), (n + 1) * (k + 1))
      prom <- matrix(prob, nrow = k + 1)
      discount <- seq(alpha1, alpha2, length = k + 1)
      delta <- (alpha2 - alpha1) / (k * k)

      for (i in 1:k) {
        discount[i + 1] = discount[i] + (2 * i - 1) * delta
      }

            for (i in 1:(length(x))) {
        for (l in 0:k) {
          for (j in 0:n) {
            prom[l + 1, j + 1] <-
              (prom[l + 1, j + 1] * (1 - discount[l + 1]) + discount[l + 1] * (1 / (n +
                                                                                      1)) * sumr[l + 1]) / sumt
          }
        }
        probparam <-  colSums(prom)
        probparam <- probparam / sum(probparam)
        sumr <- rowSums(prom)
        sumt <- sum(sumr)
        probdis <- sumr / sumt
        if (yes == 1) {
          probcondyes[i, ic] <- sum(probparam * param)
          condyeschange[i, ic] <- sum(probdis * discount)
        }
        else {
          probcondno[i, ic] <- sum(probparam * param)
          condnochange[i, ic] <- sum(probdis * discount)
        }

        if (((yes == 1) && (z[i] == 1)) || ((yes == 0) &&
                                            (z[i] == 0))) {
          if (x[i] == 1) {
            for (l in 0:k) {
              prom[l + 1, ] <- prom[l + 1, ] * param
            }
          }
          else {
            for (l in 0:k) {
              prom[l + 1, ] <- prom[l + 1, ] * (1 - param)
            }
          }
        }
      }
    }
  }

  # Classify elements
  # = 1 if 1 prior probability is used in the classification. If 0 prior
  # probability is not used (equivalent to the uniform prior probability
  good = 0
  total = nca

  for (i in (1:total)) {
    proby = log(probyes[i])
    probn = log(1 - probyes[i])

    for (j in (1:658))
    {
      if (email[i, j] == 1) {
        proby = proby + log(probcondyes[i, j])
        probn = probn + log(probcondno[i, j])
      }
      else{
        proby = proby + log(1 - probcondyes[i, j])
        probn = probn + log(1 - probcondno[i, j])
      }
    }
    if (proby > probn) {
      result = 1
    }
    else{
      result = 0
    }

    #  print(c(result,usenet[i,659]))
    if (result == email[i, nat + 1]) {
      good = good + 1
    }
  }
  success = good / total
  success
}
