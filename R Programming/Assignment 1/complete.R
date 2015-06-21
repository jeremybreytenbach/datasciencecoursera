complete <- function(directory, id = 1:332) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  n = 0;
  for(k in id)
  {
    n=n+1;
    thisData = read.csv(sprintf("%s/%s/%s.csv",getwd(),directory,formatC(k, width = 3,flag = 0)));
    nobs[n] = sum(!is.na(thisData[1:nrow(thisData),2]) & !is.na(thisData[1:nrow(thisData),3]));
  }
  
  return(data.frame(id,nobs))
}