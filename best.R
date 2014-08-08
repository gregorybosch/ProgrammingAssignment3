best <- function(state, outcome) {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv")
    states <- unique(as.vector(outcomedata$State))
    datacols <- c(2, 7)
    i <- NULL
    
    ## Check that state and outcome are valid
    if ((state %in% states) == FALSE) {
        return ("invalid state")
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
        return ("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## ratee
    outcome.s <- subset(outcomedata, outcomedata[,7] == state & outcomedata[,i] != "Not Available", select = append(datacols, i, after = length(datacols)))
    outcome.s[,3] <- sapply(outcome.s[,3], function(x) {as.numeric(as.character(x))})
    return(as.character(outcome.s[order(outcome.s[,3]),][1,1]))
}