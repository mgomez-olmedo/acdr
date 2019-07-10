
#' Saves the result of a experiment into a file in order to obtain
#' posterior data about computation
#' @param method id of method employed
#' @param iteration related to the results
#' @param args arguments used
#' @param results produces by estimation method
#' @param time execution time
#' @param realp real value of p parameter
#' @param kl value of kullback-leibler distance between estimation and
#' real value of the parameter
saveResult <- function(method, iteration, args, results, time, realp, kl){
  # limit precision for values
  options(scipen=999)

  # gets the names of the args
  argumentNames <- names(args)

  # compose string with arguments names and values
  arguments <- ""
  for(i in 1:length(argumentNames)){
    arguments <- paste0(arguments, argumentNames[i])
    arguments <- paste0(arguments, "-")
    arguments <- paste0(arguments, args[i])
    if(i < length(argumentNames)){
      arguments <- paste0(arguments, "-")
    }
  }

  # composes the filename
  filename <- paste0(method, "-")
  filename <- paste0(filename, iteration)
  filename <- paste0(filename, "-")
  filename <- paste0(filename,arguments)

  # adds the folder results
  filename <- paste0("./results/",filename)

  # add the extension
  filename <- paste0(filename, ".res")

  # saves the results as rds file
  # composes a complete list with the information
  info <- list(results=results, realp=realp, kl=kl, time=time)
  saveRDS(info, filename)

  # reset precision to normal value
  options(scipen=0)
}

#' Function that calls to the appropriate estimation procedure for a set
#' of parameters
#' @param x stream of data
#' @param param parameters for the experiment
#' @param rp value of p parameter for each sample
#' @param iteration number of iteration
sexp <- function(x, param, rp, iteration) {
  # check length of param
  if (length(param) > 0) {
    cat("----------------------------- Executing test-------------------\n")
    print(param)
    cat("Iteration: ", iteration, "\n")
    cat("---------------------------------------------------------------\n")

    # selects the method to execute according to method name (stored in
    # param[1])
    switch(
      param[1],
      '1' = v <- sexp1(x, param, rp, iteration),
      '2' = v <- sexp2(x, param, rp, iteration),
      '3' = v <- sexp3(x, param, rp, iteration),
      '4' = v <- sexp4(x, param, rp, iteration),
      '5' = v <- sexp5(x, param, rp, iteration),
      '6' = v <- sexp6(x, param, rp, iteration),
      '7' = v <- sexp7(x, param, rp, iteration),
      '8' = v <- sexp8(x, param, rp, iteration),
      '9' = v <- sexp9(x, param, rp, iteration),
      '10' = v <- sexp10(x, param, rp, iteration),
      '11' = v <- sexp11(x, param, rp, iteration),
      '12' = v <- sexp12(x, param, rp, iteration),
      '13' = v <- sexp13(x, param, rp, iteration),
      '14' = v <- sexp14(x, param, rp, iteration),
      '15' = v <- sexp15(x, param, rp, iteration),
      '16' = v <- sexp16(x, param, rp, iteration),
      '17' = v <- sexp17(x, param, rp, iteration),
      '18' = v <- sexp18(x, param, rp, iteration),
      '19' = v <- sexp19(x, param, rp, iteration),
      '20' = v <- sexp20(x, param, rp, iteration),
      '21' = v <- sexp21(x, param, rp, iteration),
      '22' = v <- sexp22(x, param, rp, iteration),
      '23' = v <- sexp23(x, param, rp, iteration),
      '24' = v <- sexp24(x, param, rp, iteration),
      '25' = v <- sexp25(x, param, rp, iteration),
      '26' = v <- sexp26(x, param, rp, iteration),
      '27' = v <- sexp27(x, param, rp, iteration),
      '28' = v <- sexp28(x, param, rp, iteration),
      '29' = v <- sexp29(x, param, rp, iteration),
      '34' = v <- sexp34(x, param, rp, iteration),
      '35' = v <- sexp35(x, param, rp, iteration),
      '36' = v <- sexp36(x, param, rp, iteration)
    )
    cat("--------------------------------------------------------------\n")

    # return the result
    return(v)
  }
}

#' read a file containing the description of an experiment and
#' executes it
#' @param name of the file describing the test to perform
#' @export
experiment <- function(name) {
  # loads all the functions in package
  devtools::load_all()

  # set seed for making all the experiments with the same
  # set of values
  set.seed(0)

  # read file with experiments description
  con <- file(name, "r")

  # gets first line. This line contains the length of the stream to
  # generate and the number of changes to produce
  line <- readLines(con, n = 1)
  param <- strsplit(line, ",")
  n <- param[[1]][1]
  cat("sample length: ", n, "\n")

  nchanges <- param[[1]][2]
  cat("number of changes: ", nchanges, "\n")

  # the second line contains the number of repetitions for the
  # experiment
  repet <- readLines(con, n = 1)
  cat("repetitions: ", repet, "\n")

  # read until the end of the file (n=-1). The rest of the lines
  # include the id of the algorithm and the parameters to use
  met <- readLines(con, n = -1)

  # close the file
  close(con)

  # gets the methods: the methods will be specified at the beginning
  # of each line
  mets <- strsplit(met, ",")
  cat("Method: \n")
  print(mets)

  # initialize vector of results
  results <- list(vector(), vector(), vector())

  # loop for repetitions
  for (i in 1:repet) {
    # generate the real values for p parameter guiding the
    # generation of the sample
    rp <- stats::runif(nchanges)

    # generate the stream to analyze of size (n * nchanges)
    x <- simulate(n, rp)

    # generate tr vector containing the real value for p for each
    # sample
    tr <- rep(rp, each = n)

    # produce the execution of the test for each method
    for (line in mets) {

      # execute the experiment
      h <- sexp(x, line, tr, i)

      # composes results
      if (length(h) > 0) {
        results <-
          list(c(results[[1]], h[[1]]),  c(results[[2]], h[[2]]), c(results[[3]], h[[3]]))
      }
    }
  }

  # gives names to results
  names(results) <- c("loglike", "method", "param")

  mres <- matrix(results[[1]], ncol = as.integer(repet))
  mmethod <- results[[2]][1:nrow(mres)]
  mparam <- results[[3]][1:nrow(mres)]

  # computes averages and return the results
  averages <- rowMeans(mres)
  print(mmethod)
  print(mparam)
  print(averages)
  return(list(mres, mmethod, mparam))
}
