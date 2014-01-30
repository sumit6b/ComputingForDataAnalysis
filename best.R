best<-function(state,outcome){
  state<-as.character(state)
  outcome<-as.character(outcome)
  
  datafile<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  causes<-list("heart attack"=11, "heart failure"= 17, "pneumonia"=23)
  
  if(!outcome %in% names(causes)){stop("invalid outcome")}
  states<-levels(factor(datafile$State))
  if(state %in% states==FALSE) {stop("invalid state")}

  ##find the hostpital with lowest death date
  options(warn=-1)
  statedata<-subset(datafile,datafile$State %in% state)
  n<-as.numeric(causes[outcome])
  statd<-as.numeric(statedata[(!is.na(as.numeric(statedata[,n]))),n])
  lowestdeath<-range((statd))[1]
  bestrows<-subset(statedata,statedata[,n] %in% lowestdeath)
  options(warn=0)
  
  #resolve ties:
  hospital<-sort(bestrows$Hospital.Name)[1]
  return (as.character(hospital))
}