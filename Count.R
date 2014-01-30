count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(is.null(cause)){stop("error")}
  
  ## Check that specific "cause" is allowed; else throw error
  causes<-list("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  if(!cause %in% causes){stop("error")}
  
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  
  ## Extract causes of death
  r<-length(grep(paste("<dd>[C|c]ause: ",cause,"</dd>", sep=""), homicides, ignore.case=TRUE))
  
  ## Return integer containing count of homicides for that cause
  return (r)
}