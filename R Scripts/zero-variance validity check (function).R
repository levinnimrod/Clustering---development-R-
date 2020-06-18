# this function checks if the responses of each participant are identical
exclude <- function(data) {
        meanvalues <- rowSums(data)/ncol(data)
        diff <- meanvalues - meanvalues
        
        diff <- data[, 1] - meanvalues + diff
        
        
        for (i in seq(ncol(data))) {
                diff <- abs(data[, i] - meanvalues) + diff
                }
        diff
}
