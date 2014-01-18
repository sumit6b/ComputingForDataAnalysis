corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
  file_vector <- list.files(directory)
  #print(file_vector)
  vec<-numeric()
  for(file in file_vector){
	file<-as.numeric(substr(file, start = 1, stop = 3))
	#print(f)
	data<-getmonitor(file,directory)
	#print(file)
	value_vec<-data[complete.cases(data),]
	if(nrow(value_vec)>threshold){
	  #print(threshold)
	  vec<-c(vec, cor(as.vector(value_vec[["nitrate"]]), as.vector(value_vec["sulfate"])))
	  
	}
  }
  #print(vec)
  return (vec)
}