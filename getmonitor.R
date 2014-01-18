getmonitor <- function(id = 001, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
        
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
        
  ## Your code here
  a<-directory
  b<-"/"
  c<-".csv"
  id<-as.numeric(id)
  stringa<-paste(a,b,sep="")
  
  ##adding the zeros infront of files:
  if(id<10){
    id<-paste("00",id,sep="")
    
  }else if(id>=10&&id<100){
	id<-paste("0",id,sep="")
		
   }
  
  stringb<-paste(stringa,id,sep="")
  final_dir<-paste(stringb,c,sep="")
   
  ##finally recevied the filename in final_dir
  filename <- final_dir
  ##print(filename)
  data <- read.csv( filename )

  if(summarize){
	print(summary(data))
  }
  return (data) 
}