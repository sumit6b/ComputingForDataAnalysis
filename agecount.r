agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age)){stop("error")}
  
  ## Read "homicides.txt" data file
  homicides<-readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  r<-length(grep(paste(age," years old",sep=""),homicides))
  ## Return integer containing count of homicides for that age
  return (r)
}