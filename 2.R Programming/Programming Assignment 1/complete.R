complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers ot be used

    ## Return a data frame of the form:
    ## id   nobs
    ## 1    117
    ## 2    1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases

    values <- data.frame(numeric(), numeric())
    for(i in id) {
        pollutantData = readData(directory, i)
        cases <- complete.cases(pollutantData)
        values <- rbind(values, c(i, sum(cases)))

    }
    colnames(values) <- c("id", "nobs")
    values

}
readData <- function(directory, fileIndex) {
    filename <- sprintf("%03d", fileIndex)
    read.csv(file.path(directory, paste(filename, ".csv", sep = "")))

}