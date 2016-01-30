rankall <- function(outcome, num) {
  outc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  res <- vector()
  states <- levels(as.factor(outc$State))
  for(s in states) {
    #print(s)
    r <- rankhospital(s, outcome, num)
    #as.data.frame(r, )
    #print(r[2])
    #df[1] <- r[2]
    #df[2] <- s
    #print(r[2])
    res <- append(res, c(r[1],s))
    #print(res)
  }
  ot <- as.data.frame(matrix(res, length(states), 2, byrow=T))
  colnames(ot) <- c("hospital", "state")
  rownames(ot) <- states
  ot
}

rankhospital <- function(state, outcome, rank) {
  outc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #ac <- outc[outc$State==state,"State"]
  #nc <- 
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  ac <- subset(outc, State == state,c(col,Hospital.Name,State))
  dc <- complete.cases(as.numeric(ac[,1]))
  ac <- ac[dc,]
  #n <- !is.na(ac[,1])
  ac0 <- order(as.numeric(ac[,1]), ac[,2])
  
  rowf <- 1
  rowf <- if (rank == "best") {
    1
  } else if (rank == "worst") {
    nrow(ac)
  } else {
    rank
  }
  #aco <- order(ac[,1])
  ac[ac0[as.numeric(rowf)],c(2,3)]
}



best <- function(state, outcome) {
  outc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #ac <- outc[outc$State==state,"State"]
  #nc <- 
  col <- if (outcome == "heart attack") {
      11
  } else if (outcome == "heart failure") {
      17
  } else {
      23
  }
  
  ac <- subset(outc, State == state,c(col,Hospital.Name,State))
  ac0 <- order(as.numeric(ac[,1]), ac[,2])
  #aco <- order(ac[,1])
  ac[ac0[1],]
}
