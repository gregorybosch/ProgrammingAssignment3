best <- function(state, outcome) {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv")
    states <- unique(as.vector(outcomedata$State))
    datacols <- c(2, 7)
    
    ## Check that state and outcome are valid
    if ((state %in% states) == FALSE) {
        return ("invalid state")
    }
    
    if (outcome = "heart attack") {
        datacols <- append(datacols, 11, after = length(datacols))
    }
    else if (outcome = "heart failure") {
        datacols <- append(datacols, 17, after = length(datacols))
    }
    else if (outcome = "pneumonia") {
        datacols <- append(datacols, 23, after = length(datacols))
    }
    else {
        return ("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## ratee
    suboutcomedata <- subset(outcomedata, outcomedata$State == state, select = datacols)
    
}