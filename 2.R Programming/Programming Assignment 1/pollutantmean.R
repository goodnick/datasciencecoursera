pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files

    ## 'poolutant' is a character vector of length 1 indicating the name of the
    ## pollutant for which we will calculate the mean;
    ## either 'sulfate' or 'nitrate'

    ## 'id' is an integer vector indicating teh monitor ID numbers ot be used

    ## Return the mean of the pollutant across all monitors list in the 'id'
    ## vector (ignoring NA values)
    ## NOTE: Do not round the result!

    sumTotal <- 0
    countTotal <- 0

    for(i in id) {
        pollutantData = readData(directory, i)
        values <- na.omit(pollutantData[[pollutant]])
        sumTotal <- sumTotal + sum(values)
        countTotal <- countTotal + length(values)

    }
    sumTotal / countTotal

}

readData <- function(directory, fileIndex) {
    filename <- sprintf("%03d", fileIndex)
    read.csv(file.path(directory, paste(filename, ".csv", sep = "")))

}