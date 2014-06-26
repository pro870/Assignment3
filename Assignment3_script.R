outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
outcome[,11]<-as.numeric(outcome[,11])
outcome[,17]<-as.numeric(outcome[,17])
outcome[,23]<-as.numeric(outcome[,23])

#function that checks if valid state 

#subset by Texas for testing

x<-subset(x,x$State == "TX")

#ordering by column 17 -- heart failure

x<-x[order(x[,17],decreasing=TRUE,na.last=NA),]

nms <- names(methods:::.BasicFunsList)