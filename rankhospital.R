rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv")
    states <- unique(as.vector(outcomedata$State))
    datacols <- c(2, 7)
    i <- NULL
    
    ## Check that state and outcome are valid
    if ((state %in% states) == FALSE) {
        stop ("invalid state")
    }
    
    if (outcome == "heart attack") {
        i <- 11
    }
    else if (outcome == "heart failure") {
        i <- 17
    }
    else if (outcome == "pneumonia") {
        i <- 23
    }
    else {
        stop ("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    outcome.s <- subset(outcomedata, outcomedata[,7] == state & outcomedata[,i] != "Not Available", select = append(datacols, i, after = length(datacols)))
    outcome.s[,3] <- sapply(outcome.s[,3], function(x) {as.numeric(as.character(x))})
    if (num == "best") {
        return (as.character(outcome.s[order(outcome.s[,3], outcome.s[,1]),][1,1]))
    }
    else if (num == "worst") {
        return (as.character(outcome.s[order(outcome.s[,3], outcome.s[,1]),][nrow(outcome.s),1]))
    }
    else if (is.numeric(num)) {
        return (as.character(outcome.s[order(outcome.s[,3], outcome.s[,1]),][num,1]))
    }
    else {
        stop ("invalid rank")
    }
}