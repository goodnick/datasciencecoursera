corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the number of
    ## completely observed observations (on all variables) required to compute
    ## the correlation between nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    corrValues <- numeric(0)

    for(i in dir(directory)) {
        pollutantData = readData(directory, i)
        cases <- complete.cases(pollutantData)

        if (sum(cases) >= threshold && sum(cases) > 0) {  ## when no observations are complete cor results in NAs.
           corrValues <- c(corrValues, cor(pollutantData$nitrate[cases] , pollutantData$sulfate[cases]))
       }

    }

    corrValues

}
readData <- function(directory, filename) {
    read.csv(file.path(directory, filename))

}