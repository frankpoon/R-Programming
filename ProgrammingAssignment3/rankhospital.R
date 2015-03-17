rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    raw_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## check that state and outcome are valid
    ## column 7: state
    validStates = unique(raw_data[,7])
    if (state %in% validStates == FALSE) {
        stop("invalid state")
    }
    if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
        stop("invalid outcome")
    }
    
    ## Return hosptial name in that state with the given 'num'
    ## column 11: 30-day death rates from heart attack
    ## column 17: 30-day death rates from heart failure
    ## column 23: 30-day death rates from pheumonia
    
    if (outcome == "heart attack") {
        col = 11
    } else if (outcome == "heart failure") {
        col = 17
    } else if (outcome == "pneumonia") {
        col = 23
    }   
    
    ## don't forget to set class type for death rate to numeric before subsetting
    ## otherwise, there will be NA death rate
    raw_data[,col] <- as.numeric(raw_data[,col])
    df <- subset(raw_data, raw_data[, 7] == state & !is.na(raw_data[, col]))
    if (num == "best") { 
        df <- subset(df, df[,col] == min(df[,col]))
        result = df[,2]
    } else if (num == "worst") {
        df <- subset(df, df[,col] == max(df[,col]))        
        result = df[,2]
    } else if (!is.na(as.numeric(num) == TRUE)) {
        numHospitals = unique(raw_data[,2])
        n = as.numeric(num)
        if (n > length(numHospitals)) {
            result <- NA
        } else {
            result = df[order(df[,col], df[,2]), 2][n]
        }
    } else {
        stop("num needs to 'best', 'worst', or a number")
    }

    result
}