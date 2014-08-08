rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv")
    states <- sort(unique(as.vector(outcomedata$State)))
    l <- length(states)
    datacols <- c(2, 7)
    dd <- data.frame(matrix(ncol = 2, nrow = l), row.names = states)
    colnames(dd) <- c("hospital","state")
    
    i <- NULL
    j <- 1
    ## Check that state and outcome are valid
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    repeat {
        outcome.s <- subset(outcomedata, outcomedata[,7] == states[j] & outcomedata[,i] != "Not Available", select = append(datacols, i, after = length(datacols)))
        outcome.s[,3] <- sapply(outcome.s[,3], function(x) {as.numeric(as.character(x))})
        if (num == "best") {
            outcome.s <- outcome.s[order(outcome.s[,3], outcome.s[,1]),]
            dd[j,] <- c(as.character(outcome.s[1,1]), states[j])
        }
        else if (num == "worst") {
            outcome.s <- outcome.s[order(outcome.s[,3], outcome.s[,1]),]
            dd[j,] <- c(as.character(outcome.s[nrow(outcome.s),1]),states[j])
        }
        else if (is.numeric(num)) {
            outcome.s <- outcome.s[order(outcome.s[,3], outcome.s[,1]),]
            if (nrow(outcome.s) < num) {
                dd[j,] <- c("<NA>",states[j])
            }
            else {
                dd[j,] <- c(as.character(outcome.s[num,1]),states[j])
            }
        }
        else {
            stop ("invalid rank")
        }
        j <- j + 1
        outcome.s <- NULL
        if (j > l) break()
    }
    return(dd)
}