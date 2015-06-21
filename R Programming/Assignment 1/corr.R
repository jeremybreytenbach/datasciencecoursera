corr <- function(directory, threshold = 0) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  id = 1:332;
  n = 0;
  corr = vector("numeric");
  for(k in id)
  {    
    completeData = complete(directory,k);
    if(completeData[1,2] > threshold)
    {
      n = n+1;
      thisData = read.csv(sprintf("%s/%s/%s.csv",getwd(),directory,formatC(k, width = 3,flag = 0)));
      
      indNotNA = !is.na(thisData[1:nrow(thisData),2]) & !is.na(thisData[1:nrow(thisData),3])
      corr[n] = cor(thisData[indNotNA,2],thisData[indNotNA,3])
    }
  }
  return(corr)
}