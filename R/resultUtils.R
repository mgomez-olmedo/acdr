library(ggplot2)
library(graphics)
library(grDevices)

#' gets the result files for a certain method and a named vector of
#' arguments.
#' Auxiliar method
#' @param method method of interest
#' @param arguments combinations of arguments of interest
#' @return list of files matching the values of the arguments
getResultFiles <- function(method, arguments){
  # gets the names of the arguments
  argumentNames <- names(arguments)

  argumentsString <- ""
  for(i in 1:length(argumentNames)){
    argumentsString <- paste0(argumentsString, argumentNames[i])
    argumentsString <- paste0(argumentsString, "-")
    argumentsString <- paste0(argumentsString, arguments[i])
    if(i < length(argumentNames)){
      argumentsString <- paste0(argumentsString, "-")
    }
  }

  # composes the name of the method
  filename <- paste0("^",method)
  filename <- paste0(filename, "-")

  # compose the pattern for matching any iteration index
  filename <- paste0(filename, ".*")
  filename <- paste0(filename, "-")
  filename <- paste0(filename,argumentsString)
  filename <- paste0(filename,".res")

  # this filename will be used as pattern
  files <- list.files(path="./results", filename)

  # return the list of files matching the arguments
  files
}

#' get the complete list of values for a certain method
#' Auxiliar method
#' @param method method of interest
#' @return list of files matching the arguments
#' @export
getFilesForMethod <- function(method){
  # compose the pattern for the file name
  fileNamePattern <- paste0("^", method)
  fileNamePattern <- paste0(fileNamePattern, "-")
  fileNamePattern <- paste0(fileNamePattern, ".*")
  files <- list.files(path="./results", fileNamePattern)
}

#' remove the extension of a list of files with results
#' Auxiliar method
#' @param filenamesList list of file names
#' @return list of files after removing extensions
removeExtensionOfFileNamesList <- function(filenamesList){
  lapply(filenamesList, function(y){
    tools::file_path_sans_ext(y)
  })
}

#' get the names of the arguments composing the name of a file with
#' results
#' Auxiliar method
#' @param filename name of the file of interest
#' @return list of argument names
getArgumentNames <- function(filename){
  # remove the extension of the filename
  fileBaseName <- tools::file_path_sans_ext(filename)

  # split the filaneme using - as separator
  strings <- strsplit(fileBaseName, "-")

  # the first argument will be the identifier of the method
  # and the second the identifier of the repetition. The
  # arguments will be from position 3 to length of strings
  # minus 1, and adding 2 from one to another
  indices <- seq(3,length(strings[[1]]),2)

  # gets names for the selected indices
  argumentNames <- strings[[1]][indices]
}

#' get the values of the arguments composing the name of a file with
#' results
#' Auxiliar method
#' @param filename name of the file to consider
#' @return list of values for the arguments
getArgumentValues <- function(filename){
  # remove the extension of the fileName
  fileBaseName <- tools::file_path_sans_ext(filename)

  # split the filaneme using - as separator
  strings <- strsplit(fileBaseName, "-")

  # the first argument will be the identifier of the method
  # and the second the identifier of the repetition. The values
  # of the arguments will be from position 4 to length of strings
  # and adding 2 from one to another
  indices <- seq(4,length(strings[[1]]),2)

  # gets names for the selected indices
  argumentValues <- strings[[1]][indices]
}

#' get a complete vector of values for a certain argument in a
#' collection of files containing results
#' Auxiliar method
#' @param filenames list of files to analyze
#' @param argumentName name of the argument to look for
#' @return vector of values for the argument considered in
#' the list of file names
getValuesForArgument <- function(filenames, argumentName){
  # removes file extensions
  filesWithoutRes <- removeExtensionOfFileNamesList(filenames)

  # get the argument names
  argumentNames <- getArgumentNames(filenames[[1]])

  # gets the position of argumentName in argumentNames
  indexOfArgumentName <- which(argumentName == argumentNames)

  # gets the index of the argument into the split of the file name
  indexInFileName <- 2*indexOfArgumentName+1

  # for each filename split the name and store the part with
  # index given by indexInFileName+1
  values <- c()
  values <- lapply(filesWithoutRes, function(filename){
    # split the name
    strings <- strsplit(filename, "-")
    strings[[1]][indexInFileName+1]
  })

  # remove duplicates and return the result as vector
  unlist(unique(values))
}

#' method for classifying the files related to a certain method
#' according to their parameters
#' Auxiliar method
#' @param method algorithm to analyze
#' @return list with a number of entries equals to the number
#' of combinations of parameters for the arguments of the
#' method of interest
classifyResultFilesForMethod <- function(method){
  # gets the files for this method
  files <- getFilesForMethod(method)

  # gets argument names using one of the names
  argumentNames <-getArgumentNames(files[[1]])

  # gets the values for each argument
  valuesForArguments <- sapply(argumentNames, function(argument){
    getValuesForArgument(files, argument)
  })

  # the result is a named vector and for each name there is a list
  # of possible values. Get the combinations of both of them
  combinations <- expand.grid(valuesForArguments)
  if(ncol(combinations) == 1){
    colnames(combinations) <- argumentNames
  }

  # combinations is a data.frame and we need to recover all the file
  # names for each combination of arguments and values (each row of
  # the data frame)
  res <- apply(combinations, 1, function(combination){
    # gets the files for this combination
    files <- getResultFiles(method, combination)

    # compose a vector for each combination
    list("arguments"=combination, "files"=files)
  })

  # return res: a list with a number of entries equal to the number
  # of combinations: each entry contains a given combination of
  # values for the parameters and a list of files corresponding to
  # this parameterization
  res
}

#' compose a pattern containing a method name and a combination
#' of arguments
#' Auxiliar method
#' @param method method of interest
#' @param combination combination of parameters and values
#' @return name of the file for the algorithm of interest and
#' the parameterization specified with the second argument
composePatternName <- function(method, combination){
  pattern <- ""
  # add method to pattern
  pattern <- paste0(method, "-")

  # paste the wildcard for the iteration number
  pattern <- paste0(pattern, ".?-")

  # now paste the complete lust of arguments in order
  for(i in 1:length(combination)){
    pattern <- paste0(pattern,names(combination)[i])
    pattern <- paste0(pattern,"-")
    pattern <- paste0(pattern, unname(combination[i]))
    if(i != length(combination)){
      pattern <- paste0(pattern, "-")
    }
  }

  # return pattern
  pattern
}

#' generate a plot for a method with the results stored in a certain
#' file
#' @param method method of interest
#' @param filename name of the file to analyze
#' @export
generatePlot <- function(method, filename){
  # gets the argument name
  argumentNames <- getArgumentNames(filename)

  # gets argument values
  argumentValues <- getArgumentValues(filename)

  # read the data of the file
  filePath <- paste0("./results/",filename)
  data <- readRDS(file=filePath)

  # generate the plot for the estimation and the real value
  # of the parameter
  label <- paste0("method ", method)
  label <- paste(label, makeLabelWithParameters(filename), sep=" - ")
  graphics::plot(data$results$estimate,type="l", col="red", lwd=3,
                 xlab="samples", ylab="parameter", ylim=c(0,1), main=label)
  graphics::lines(data$realp, type="l", col="blue", lwd=3)
  graphics::legend("topleft", legend=c("Estimation", "Real value"), col=c("red", "blue"),
                   lty=1:2, cex=0.8, box.lty=0)
}

#' method for preparing the label to include in a graphic from the
#' name of a file
#' Auxiliar method
#' @param filename name of the file of interest
makeLabelWithParameters <- function(filename){
  # gets the argument name
  argumentNames <- getArgumentNames(filename)

  # gets argument values
  argumentValues <- getArgumentValues(filename)

  # compose a label with all of them
  string <- paste(argumentNames, argumentValues, sep="=", collapse = " ")
}

#' plots the results considering a list of result files and taking
#' into account a certain parameter
#' @param method method of interest
#' @param listOfFiles list of files to process
#' @param valueName name of the argument used for grouping results
#' @export
plotResultForCombination <- function(method, listOfFiles, valueName){
  # get the list of arguments
  arguments <- listOfFiles[[1]]$arguments

  # get the list of files
  files <- listOfFiles[[1]]$files

  # read the files
  data <- lapply(files, function(filename){
    filePath <- paste0("./results/",filename)
    data <- readRDS(file=filePath)
  })

  # gets KL average and time averages
  kl <- sapply(data, function(dataResult){
    dataResult$kl
  })

  time <- sapply(data, function(dataResult){
    dataResult$time
  })

  # gets the maximum value for KL
  maxkl <- max(kl)
  plot(kl,type="l", col="red", lwd=2,
       xlab="iterations", ylab="KL",
       ylim=c(0,maxkl), main=paste0("method ",method))

  maxtime <- max(time)
  plot(time,type="l", col="blue", lwd=2,
       xlab="iterations", ylab="Time",
       ylim=c(0,maxtime), main=paste0("method ",method))
}

#' process a list of files reading their content and computing KL and time
#' statistisc. The result is a list with entries for: arguments, klMean,
#' klStedev, timeMean and timeStdev
#' @param listOfFiles list of files as produced by classifyResultsByMethod
#'                    (a list of results for each combination of arguments)
#' @return list with entries for arguments, klMean, klStdev, timeMean and
#' timeStdev
processResultFiles <- function(listOfFiles){
  # get the list of arguments
  arguments <- listOfFiles$arguments

  # get the list of files
  files <- listOfFiles$files

  # read the files
  data <- lapply(files, function(filename){
    filePath <- paste0("./results/",filename)
    data <- readRDS(file=filePath)
  })

  # gets KL average
  kl <- sapply(data, function(dataResult){
    dataResult$kl
  })

  klMean <- mean(kl)
  klStdev <- sd(kl)

  # gets time average
  time <- sapply(data, function(dataResult){
    dataResult$time/1000000000.0
  })

  timeMean <- mean(time)
  timeStdev <- sd(time)

  # return a list with an entry for each value
  list("arguments"=arguments, "klMean"=klMean, "klStdev"=klStdev, "timeMean"=timeMean, "timeStdev"=timeStdev)
}

#' method for processing the files for a given method
#' @param method algorithm of interest
#' @return a list for each combination of result files for
#' each combination of arguments. Each entry list contains:
#' arguments, klMean, KlStdev, timeMean and timeStdev
processResultFilesMethod <- function(method){
  # classify the files for this method
  classifiedFiles <- classifyResultFilesForMethod(method)

  # now process the result files for each variant
  lapply(classifiedFiles, function(listFiles){
    processResultFiles(listFiles)
  })
}

#' gets the matrix of results for a given algorithm
#' @param method algorithm of interest
#' @return matrix with result for the files of results related to a
#' certain method. The columns of the matrix will contain information
#' about klMean, klStdev, timeMean and timeStdev
getResultsMatrix <- function(method){
  # process the result files for the method
  results <- processResultFilesMethod(method)

  # gets the arguments
  args <- lapply(results, function(result){
    result$arguments})

  # convert the results into a matrix of a number of rows equal to
  # the combinations of arguments and a number of columns given by
  # the argument values plus 4 columns for KL and time averages and
  # standard deviations
  argsMatrix <- matrix(nrow=length(args), ncol=length(args[[1]])+4)
  # order the results according to parameter values
  for(i in 1:length(args)){
    for(j in 1:length(args[[1]])){
      argsMatrix[i,j] <- as.numeric(unname(args[[i]][j]))
    }

    # assign values for kl and times
    column <- length(args[[1]])
    argsMatrix[i,column+1] <- results[[i]]$klMean
    argsMatrix[i,column+2] <- results[[i]]$klStdev
    argsMatrix[i,column+3] <- results[[i]]$timeMean
    argsMatrix[i,column+4] <- results[[i]]$timeStdev
  }

  # sets colnames
  colnames(argsMatrix) <- c(names(args[[1]]),"klMean", "klStdev",
                            "timeMean", "timeStdev")

  # order according to argument values if needed
  if(nrow(argsMatrix) > 1){
    argsMatrix[do.call(order, as.data.frame(argsMatrix)),]
  }

  # return argsMatrix
  argsMatrix
}

#' compounds a string with the method and the concatenation of method,
#' argument names and corresponding values. Tha args are specified throw
#' selecting from the matrix of results the columns of a row containing
#' the values
#' @param method algorithm of interest
#' @param args argument names and values
makeString <- function(method, args){
  string <- paste0(method, "-")
  for(i in 1:length(args)){
    string <- paste0(string,names(args[i]))
    string <- paste0(string, "-")
    string <- paste0(string, args[i])
    if(i < length(args)){
      string <- paste0(string, "-")
    }
  }
  string
}

#' method for generating a latex report with the results for a certain
#' method of interest. The documents will be placed in a folder named
#' resultDocus
#' @param method algorithm of interest
#' @export
generateLatexReport <- function(method){
  # sets the name of the folder where the documents will
  # be generated
  folder <- "./resultDocus/"

  # gets the matrix with the results
  matrix <- getResultsMatrix(method)
  cat("Number of rows in matrix: ",nrow(matrix),"\n")

  # determine the columns of args and results
  argsColumns <- 1:(ncol(matrix)-4)
  resultColumns <- (ncol(matrix)-3):ncol(matrix)

  # compound the name of the file
  if(!dir.exists(folder)){
    cat("Creating folder named ", folder, "\n")
    dir.create(folder)
  }

  filename <- paste0(folder, method)
  filename <- paste0(filename, "-results.tex")
  cat("Generating file name: ", filename, "\n")

  # get the values for the last argument for dividing the table
  # if required
  valuesForLastArgument <- getValuesForLastArgument(matrix)
  numberValues <- length(valuesForLastArgument)

  # define the max number of rows per page
  maxLinesPage <- 40

  # determine the number of blocks: a block will contain all the
  # values for a given combination of all the arguments except the
  # last one
  blocks <- nrow(matrix)/numberValues

  # determine the number of blocks per page
  blocksPerPage <- floor(maxLinesPage/numberValues)
  if(blocksPerPage < 1){
    blocksPerPage=1
  }

  # determine the final number of lines per table
  linesTable <- blocksPerPage*numberValues

  # compute the number of tables to compute
  tables <- ceiling(nrow(matrix)/linesTable)

  # sends output to filename
  sink()
  sink()
  sink(filename)
  options(digits=6)

  # generates the preamble of the document
  makeDocumentPreamble(method)

  # defines the section for relevant information
  cat("\\section{Relevant information}\n")

  # gets the row corresponding to the algorithm with
  # min value for KL distance
  rowMinKl <- getRowMinKL(matrix)

  # shows information about the algorithm with best KL
  cat("\n\\subsection*{Algorithm with min. KL} \n")
  cat(makeString(method, matrix[rowMinKl,argsColumns]), "\n\n",
      "\\medskip\n")
  cat("\\begin{itemize}\n")
  cat("\\item KL avg.: ", matrix[rowMinKl, resultColumns[1]], "\n")
  cat("\\item KL desv.: ", matrix[rowMinKl, resultColumns[2]], "\n")
  cat("\\item Time avg.: ", matrix[rowMinKl, resultColumns[3]], "\n")
  cat("\\item Time desv.: ", matrix[rowMinKl, resultColumns[4]], "\n")
  cat("\\end{itemize}\n")

  # gets the row for the algorithm with max value of KL distance
  rowMaxKl <- getRowMaxKL(matrix)

  # shows information about the algorithm with worst KL
  cat("\n\\subsection*{Algorithm with max. KL} \n")
  cat(makeString(method, matrix[rowMaxKl,argsColumns]), "\n\n",
      "\\medskip\n")
  cat("\\begin{itemize}\n")
  cat("\\item KL avg.: ", matrix[rowMaxKl, resultColumns[1]], "\n")
  cat("\\item KL desv.: ", matrix[rowMaxKl, resultColumns[2]], "\n")
  cat("\\item Time avg.: ", matrix[rowMaxKl, resultColumns[3]], "\n")
  cat("\\item Time desv.: ", matrix[rowMaxKl, resultColumns[4]], "\n")
  cat("\\end{itemize}\n")

  # gets information about the row with min computation time
  rowMinTime <- getRowMinTime(matrix)

  # shows information about algorithm with min computation time
  cat("\n\\subsection*{Algorithm with min. computation time} \n")
  cat(makeString(method, matrix[rowMinTime,argsColumns]), "\n\n",
      "\\medskip\n")
  cat("\\begin{itemize}\n")
  cat("\\item KL avg.: ", matrix[rowMinTime, resultColumns[1]], "\n")
  cat("\\item KL desv.: ", matrix[rowMinTime, resultColumns[2]], "\n")
  cat("\\item Time avg.: ", matrix[rowMinTime, resultColumns[3]], "\n")
  cat("\\item Time desv.: ", matrix[rowMinTime, resultColumns[4]], "\n")
  cat("\\end{itemize}\n")

  # gets information about the row with maximum computation time
  rowMaxTime <- getRowMaxTime(matrix)

  # shows information about algorithm with max computation time
  cat("\n\\subsection*{Algorithm with max. computation time} \n")
  cat(makeString(method, matrix[rowMaxTime,argsColumns]), "\n\n",
      "\\medskip\n")
  cat("\\begin{itemize}\n")
  cat("\\item KL avg.: ", matrix[rowMaxTime, resultColumns[1]], "\n")
  cat("\\item KL desv.: ", matrix[rowMaxTime, resultColumns[2]], "\n")
  cat("\\item Time avg.: ", matrix[rowMaxTime, resultColumns[3]], "\n")
  cat("\\item Time desv.: ", matrix[rowMaxTime, resultColumns[4]], "\n")
  cat("\\end{itemize}\n")

  # order the rows according to KL mean
  if(nrow(matrix) > 1){
    matrix <- matrix[order(matrix[,"klMean"]),]
  }

  # shows the section for the tables
  cat("\n\n\\section{Tables with results}\n")

  # generatethe tables
  for(i in 1:tables){
    # determine the first index of the results to show for this table
    firstIndex <- (i-1)*linesTable+1
    lastIndex <- i*linesTable
    if(lastIndex > nrow(matrix)){
      lastIndex <- nrow(matrix)
    }

    # generate table for blocks
    generateTable(method, matrix, argsColumns, resultColumns,
                  firstIndex, lastIndex)

    # generate a page break for all the tables but the last one
    if(i < tables){
      cat("\n\n\\newpage\n\n")
    }
  }

  # generate the final mark of the document
  makeDocumentEnd()

  sink()
  sink()

  # return the matrix
  matrix
}

#' auxiliar method for generating a table with the results contained in the
#' matrix for a certain set of cells. It is called from the previous one
#' @param method algorithm of interest
#' @param matrix matrix with results
#' @param argsColumns columns of interest in matrix
#' @param resultColumns columns of results to consider
generateTable <- function(method, matrix, argsColumns, resultColumns,
                          firstIndex, lastIndex){
  # compose the header
  makeTableHeader()

  # writes the titles in the first row
  cat("method id & kl. av. & kl. std. & time av. & time std. \\\\\\hline\n")

  # prints the result. First at all compound the name identifier from method
  # and args description
  argsColumns <- 1:(ncol(matrix)-4)
  resultColumns <- (ncol(matrix)-3):ncol(matrix)

  for(i in firstIndex:lastIndex){
    # make the string for the method id
    methodId <- makeString(method, matrix[i,argsColumns])

    cat(methodId, "& ")

    # now get the results
    for(j in resultColumns){
      cat(matrix[i,j], " ")
      if(j < ncol(matrix)){
        cat(" & ")
      }
    }

    # adds a new line
    cat("\\\\\\hline\n")
  }

  # finishes table description
  makeTableEnd()
}

#' gets the values for the last argument from matrix
#' Auxiliar method called from generateLatexReportMethod
#' @param matrix matrix with results
getValuesForLastArgument <- function(matrix){
  # return the values in the last column of arguments
  colLastArgument <- ncol(matrix)-4

  # selects the corresponding column and return its values
  values <- unique(matrix[,colLastArgument])
}

#' gets the row with the min value for KL
#' @param matrix matrix with results
getRowMinKL <- function(matrix){
  # get the index of the column for KL
  indexKL <- ncol(matrix)-3

  # get the minimum value
  minKL <- min(matrix[,indexKL])

  # get the corresponding index
  indexMinKL <- which(matrix[,indexKL] == minKL)
}

#' gets the row with the max value for KL
#' Auxiliar method called from generateLatexReportMethod
#' @param matrix matrix with results
getRowMaxKL <- function(matrix){
  # get the index of the column for KL
  indexKL <- ncol(matrix)-3

  # get the maximum value
  maxKL <- max(matrix[,indexKL])

  # get the corresponding index
  indexMaxKL <- which(matrix[,indexKL] == maxKL)
}

#' gets the row with the min value for computation time
#' Auxiliar method called from generateLatexReportMethod
#' @param matrix matrix with results
getRowMinTime <- function(matrix){
  # get the index of the column for time
  indexTime <- ncol(matrix)-1

  # get the minimum value
  minTime <- min(matrix[,indexTime])

  # get the corresponding index
  indexMinTime <- which(matrix[,indexTime] == minTime)
}

#' gets the row with the max value for computation time
#' @param matrix matrix with results
getRowMaxTime <- function(matrix){
  # get the index of the column for time
  indexTime <- ncol(matrix)-1

  # get the maximum value
  maxTime <- max(matrix[,indexTime])

  # get the corresponding index
  indexMaxTime <- which(matrix[,indexTime] == maxTime)
}

#' compuses the preamble of the latex document with the
#' results
#' Auxiliar method called from generateLatexReportMethod
#' @param method algorithm of interest
makeDocumentPreamble <- function(method){
  cat("\\documentclass[12pt]{article}\n")
  cat("\\usepackage[utf8]{inputenc}\n")
  cat("\\usepackage{geometry}\n")
  cat("\\geometry{a4paper}\n")
  cat("\\usepackage{graphicx}\n")
  cat("\\usepackage{booktabs}\n")
  cat("\\usepackage{array}\n")
  cat("\\usepackage{paralist}\n")
  cat("\\usepackage{verbatim}\n")
  cat("\\usepackage{subfig} \n")
  cat("\\usepackage{fancyhdr}\n")
  cat("\\pagestyle{fancy} \n")
  cat("\\usepackage{sectsty}\n")
  cat("\\usepackage[nottoc,notlof,notlot]{tocbibind}\n")
  cat("\\usepackage[titles,subfigure]{tocloft}\n")
  cat("\\renewcommand{\\cftsecfont}{\\rmfamily\\mdseries\\upshape}\n")
  cat("\\renewcommand{\\cftsecpagefont}{\\rmfamily\\mdseries\\upshape}\n")
  cat("\n")
  cat("\\title{Results for method ", method, "}\n")
  cat("\n")
  cat("\\begin{document}\n")
  cat("\\maketitle\n")
}

#' composes the final labels of the document
#' Auxiliar method called from generateLatexReportMethod
makeDocumentEnd <- function(){
  cat("\n\n\\end{document}\n")
}

#' composes the header part of a table
#' Auxiliar method called from generateLatexReportMethod
makeTableHeader <- function(){
  # compose the header
  cat("\\begin{table}[h!]\n")
  cat("\\centering\n")
  cat("\\begin{tabular}{|c|c|c|c|c|}\\hline\n")
}

#' composes the final part of the table
#' Auxiliar method called from generateLatexReportMethod
makeTableEnd <- function(){
  # finishes table description
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

#' plots the results for a certain method according to a given
#' parameter
#' @param method algorithm of interest
#' @param valueName name of the data to visualize (it should be
#' klMean, klStdev, timeMean or timeMean)
plotSerieResults <- function(method, valueName){
  # get the matrix of results for the method
  matrix <- getResultsMatrix(method)

  # get the index of the column with valueName
  valueIndex <- which(colnames(matrix) == valueName)

  # gets the maximum value
  max <- max(matrix[, valueIndex])
  plot(matrix[, valueIndex],type="l", col="red", lwd=2, xaxt = "n",
       xlab="Values for argument", ylab=valueName,
       ylim=c(0,max), main=paste0("method ",method))

  # gets the values for the last argument
  valuesLastArgument <- getValuesForLastArgument(matrix)
  nvalues <- length(valuesLastArgument)

  # determine
  graphics::axis(side=1,at=1:nvalues, labels=valuesLastArgument)

  # gets the marks for the vertical lines marking the blocks
  blocksTics <- valuesLastArgument

  # draw ticks lines
  for(i in 1:nvalues){
    graphics::abline(v=i, col="blue", lty=2, lwd=1)
  }
}

#' generates ggplot graphics for a matrix of results
#' @param matrix matric with results
#' @param xAttrib value for X axis
#' @param yAttrib value for Y axis
plotSerieResultsGgplot <- function(matrix, xAttrib, yAttrib){
  # convert it into a data frame
  df <- as.data.frame(matrix)

  # presents the plot
  ggplot2::ggplot(df,aes_string(x=xAttrib,y=yAttrib)) +
    geom_line(col="red")
}

#' generate plots for the results stored in the matrix and grouping
#' them respect to a given parameter. It allows to show the influence
#' between several parameters
#' Auxiliar method
#' @param matrix matrixWithResults
#' @param groupingByAttrib attribute used for grouping results
#' @param xAttrib value for X axis
#' @param yAttrib value for Y axis
plotSeparatedSerieResults <- function(matrix, groupingByAttrib,
                                      xAttrib, yAttrib){
  # convert it into a data frame
  df <- as.data.frame(matrix)

  # convert column to be used as grouping into factor
  df[,groupingByAttrib] <- as.factor(df[,groupingByAttrib])

  # presents the plot
  ggplot(df,aes_string(x=xAttrib,y=yAttrib,colour=groupingByAttrib,
                       group=groupingByAttrib)) +
    geom_line()
}

#' gets the results as a data frame
#' Auxiliar method
#' @param method algorithm of interest
#' @return data frame with results
getResultsDataFrame <- function(method){
  # gets data
  matrix <- getResultsMatrix(method)

  # gets indices for argument columns
  argsColumns <- 1:(ncol(matrix)-4)

  # compose method ids
  names <- c()
  for(i in nrow(matrix)){
    # make the string for the method id
    names[i] <- makeString(method, matrix[i,argsColumns])
  }

  # converts into a data frame
  df <- as.data.frame(matrix)

  # changes first column to introduce mathod ids
  df[,1] <- names

  # change colnames
  columnNames <- colnames(df)
  columnNames[1] <- "method-id"
  colnames(df) <- columnNames

  # return df
  df
}

#' gets the results as a data frame using a certain criteria
#' for selecting results
#' Auxiliar method
#' @param method algorithm of interest
#' @param referenceCriteria criteria used for selection
#' @param toSelect number of rows to select
#' @return data frame with results
getResultsDataFrame <- function(method, referenceCriteria, toSelect){
  # gets data
  matrix <- getResultsMatrix(method)

  # gets column id for referenceCriteria
  index <- which(colnames(matrix) == referenceCriteria)

  # order results according to referenceCriteria
  matrix <- matrix[order(matrix[,index]),]

  # selects top toSelect combinations
  matrix <- matrix[1:toSelect, ]

  # gets indices for argument columns
  argsColumns <- 1:(ncol(matrix)-4)

  # compose method ids
  names <- c()
  for(i in 1:nrow(matrix)){
    # make the string for the method id
    names[i] <- makeString(method, matrix[i,argsColumns])
  }

  # converts into a data frame
  df <- as.data.frame(matrix)

  # changes first column to introduce mathod ids
  df[,1] <- names

  # change colnames
  columnNames <- colnames(df)
  columnNames[1] <- "method-id"
  colnames(df) <- columnNames

  # return df
  df
}

#' generate a matrix with the results of the algorithm
#' given by method and the arguments indicated by the
#' second argument. It will produce a matrix with a colum
#' (with the name of the algorithm) and the rows will
#' be the results of the runs executed for it
#' Auxiliar method (called from generateGlobalMatrixForTest)
#' @param method algorithm of interest
#' @param arguments parameterization of interest
#' @return list with KL and time values
generateMethodMatrixForTest <- function(method, arguments){
  # disable scientific notation
  options(scipen=999)

  # get the files for this combination
  files <- getResultFiles(method, arguments)
  cat("    related files: ", length(files), "\n")

  # read the files and extract values for kl and time
  data <- sapply(files, function(filename){
    filePath <- paste0("./results/",filename)
    dataInFile <- readRDS(file=filePath)
    # compose a vector with kl and time
    c(dataInFile$kl, dataInFile$time)
  }, USE.NAMES = FALSE, simplify=TRUE)
  print(data)

  # give data the corresponding format: two tables with kl and time
  # info: the data of each iteration will be stored row by row
  datakl <- data[1,]
  datatime <- data[2,]

  # convert into matrices
  datakl <- matrix(datakl,nrow=100, ncol=1)
  datatime <- matrix(datatime, nrow=100, ncol=1)

  # compose the name for the column
  argumentsString <- ""
  # gets the names of the arguments
  argumentNames <- names(arguments)
  for(i in 1:length(argumentNames)){
    argumentsString <- paste0(argumentsString, argumentNames[i])
    argumentsString <- paste0(argumentsString, "-")
    argumentsString <- paste0(argumentsString, arguments[i])
    if(i < length(argumentNames)){
      argumentsString <- paste0(argumentsString, "-")
    }
  }

  # compose the method
  argumentsString <- paste(method,argumentsString,sep="-")

  # sets colname
  colnames(datakl) <- argumentsString
  colnames(datatime) <- argumentsString
  list(kl=datakl,time=datatime)
}

#' generates a global matrix of results for a set of methods and
#' parameterizations
#' @param methods list of algorithms of interest
#' @param argumentsForMethods list of arguments for selected methods
#' @return list with kl and time values
generateGlobalMatrixForTest <- function(methods, argumentsForMethod){
  # create the matrix of results
  resultKl <- matrix(nrow=100, ncol=length(methods))
  resultTime <- matrix(nrow=100, ncol=length(methods))
  names <- c()

  # apply the function of matrix extraction for each algorithm
  cat("methods to process: ", length(methods),"\n")
  for(i in 1:length(methods)){
    cat("Getting results for method: ", methods[[i]], "\n")
    cat("and arguments: \n")
    print(argumentsForMethod[[i]])
    resultOfMethod <- generateMethodMatrixForTest(methods[[i]], argumentsForMethod[[i]])
    resultKl[, i] <- resultOfMethod$kl[,1]
    resultTime[, i] <- resultOfMethod$time[,1]
    names <- c(names, colnames(resultOfMethod$kl))
    cat("end of iteration for index ", i, "\n")
  }
  colnames(resultKl) <- names
  colnames(resultTime) <- names
  list(kl=resultKl, time=resultTime)
}

#' method for computing the global statistics about a concrete algorithm and
#' a certain combination of parameters
#' @param algorithm of interest
#' @param arguments parameterizations to consider
#' @return list with average and std. deviation for KL and time
computeStatistics <- function(method, arguments){
  # gets the corresponding list with matrices for KL and time
  listResult <- generateMethodMatrixForTest(method, arguments)

  # gets the matrix for kl
  matkl <- listResult$kl

  # gets the matrix with times (divide to get seconds)
  mattime <- listResult$time/1000000000

  # computes averages and stds
  list(avgKL=mean(matkl), sdKL=sd(matkl), avgTime=mean(mattime), sdTime=sd(mattime))
}



#' generates the matrix results for the experiments presented in the
#' paper
#' @export
generateMatrixForPaper <- function(){
  # specify methods and arguments of interest
  methods <- c(34, 34, 34, 35, 36, 4, 3, 7, 8, 11, 15, 21, 23, 24)
  algorithms <- c("BAF01", "BAF001", "BAF0001", "BFV1", "BFV2", "FW68", "FF", "SWB", "SWF", "ADWIN", "SWMTF", "SWMTFIn", "SWMTB", "SWMTBIn")

  arguments <- list(
    c(n=100, alpha=0.01),
    c(n=100, alpha=0.001),
    c(n=100, alpha=0.0001),
    c(n=100, alpha1=0.0001, alpha2=0.01, k=10),
    c(n=100, alpha1=0.0001, alpha2=0.01, k=10),
    c(n=68),
    c(ro=0.97),
    c(n=20, alpha=0.04),
    c(n=28, alpha=0.006),
    c(delta=0.2),
    c(alpha=0.01),
    c(n=5, alpha=0.01),
    c(alpha=0.06),
    c(n=5, alpha=0.008)
  )

  # gets global matrix for selected methods and arguments
  matrix <- generateGlobalMatrixForTest(methods, arguments)

  # change names of columns and set them accroding to algorithm
  # names
  colnames(matrix$kl) <- algorithms
  colnames(matrix$time) <- algorithms

  # dive times in order to get seconds
  matrix$time <- matrix$time/1000000000.0

  # store files in csv files
  write.table(matrix$kl, file="./resultDocus/kLTable", col.names=TRUE, quote=FALSE,
              row.names=FALSE, sep=",", dec=".")
  write.table(matrix$time, file="./resultDocus/timeTable", col.names=TRUE, quote=FALSE,
              row.names=FALSE, sep=",", dec=".")
}

#' generates graphics for methods with a single argument
#' @param method method of interest
#' @param argumentName name of the argument
#' NOTE: methods with a single argument
#' 3, ro, 23 different tested values
#' 4, n, 76 possible values
#' 15, alpha, 28 possible values
#' 23, alpha, 28 possible values
#' @export
generateGraphicsForMethodsWithSingleArgument <- function(method, argumentName){
  # generate the result matrix
  matrix <- getResultsMatrix(method)

  # generate the plot for klMean
  plot <- plotSerieResultsGgplot(matrix, argumentName,"klMean")

  # compose file name
  basefilename <- paste0("./resultDocus/images/",method)
  filename <- paste0(basefilename, "-klMean.pdf")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # how for timeMean
  plot <- plotSerieResultsGgplot(matrix, "ro","timeMean")
  filename <- paste0(basefilename, "-timeMean.pdf")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # remove matrix for method
  rm(matrix)
}

#' generates graphics for methods with two arguments
#' @param method method of interest
#' @param arg1 name of the first argument
#' @param arg2 name of the second argument
#' NOTE: algorithms with these features:
#' 7: n and alpha, 540 combinations
#' 8: n and alpha, 720 combinations
#' 21: n and alpha, 648 combinations
#' 22: n and alpha, 396 combinations
#' 24: n and alpha, 648 combinations
#' 25: n and alpha, 396 combinations
#' 26: n and alpha, 336 combinations
#' 27: n and alpha, 336 combinations
#' 28: n and alpha, 336 combinations
#' 34: n and alpha, 294 combinations
generateGraphicsForMethodsWithTwoArguments <- function(method, arg1, arg2){
  # gets the matrix with results
  matrix <- getResultsMatrix(7)

  # generate the plot for the first argument grouping by the second
  # for klMean
  # compose file name
  basefilename <- paste0("./resultDocus/images/",method)
  filename <- paste0(basefilename, "-klMean1.pdf")
  plot <- plotSeparatedSerieResults(matrix, arg1, arg2, "klMean")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # plot for klMean grouping by the first argument
  plot <- plotSeparatedSerieResults(matrix, arg2, arg1, "klMean")
  filename <- paste0(basefilename, "-klMean2.pdf")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # generate the plot fot timeMean grouping by the first arg
  plot <- plotSeparatedSerieResults(matrix, arg1, arg2, "timeMean")
  filename <- paste0(basefilename, "-timeMean1.pdf")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # plot for timeMean grouping by alpha
  plot <- plotSeparatedSerieResults(matrix, "alpha", "n", "timeMean")
  filename <- paste0(basefilename, "-timeMean2.pdf")
  grDevices::pdf(file=filename)
  print(plot)
  grDevices::dev.off()

  # removes matrix
  rm(matrix)
}























