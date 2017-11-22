# Clean the data using the quartiles 
# Write the function to clean the data.
# Remove the ouliers
# Find the first quantile and 3rd quantile
# Inner quantile range(IQR) = 3rd quantile - 1st quantile
# Find 1.5*IQR
# Min value can't be less than (first quantile - 1.5*(IQR))
# Max value can't bigger than (third quantile +1.5*(IQR))

removeOutliers = function(x){
 j =1
 y = 0
 
 #Checks if the given data is numeric or not 
 #If the data is not numeric then it asks the user to put numeric data
  if(class(x) == 'numeric' & length(x) != 0 | class(x)=='integer'){
    for(i in x){
      if(!is.na(i)){
        y[j] = i
        j =j+1
      }
    }
    first_quartile = quantile(y, 0.25)
    third_quartile = quantile(y, 0.75)
    Inner_Quartile_Range = third_quartile - first_quartile
    leveler = 1.5*Inner_Quartile_Range
    minleveler = first_quartile - leveler
    maxleveler = third_quartile + leveler
    
    output = NULL
    for(i in 1:length(y)){
      if(y[i]> minleveler & y[i] < maxleveler ){
        output[i] = y[i]
      }
    }
    return(output)
  }else{
    print("The input data should be numeric or integer. ")
    }
}


#This function will remove the NAs at the positions of the outliers and then putput the mean
#of the data without outliers.
ourmean = function(x){
  j=1
  y = 0
  out = removeOutliers(x)
  
  for(i in out){
    if(!is.na(i)){
      y[j] = i
      j =j+1
    }
  }
  return(mean(y))
  
}




#Function to produce the kernel density plots with the mean of the data as bandwidth

kfunction = function(x){
  out = removeOutliers(x)
  
  y = 0
  j=1
  for(i in out){
    if(!is.na(i)){
      y[j] = i
      j =j+1
    }
  }
  m = ourmean(y)
  par(mfrow =c(2,3))
  plot(density(y, kernel = "gaussian", bw = m), col = "red", main = "Gaussian K")
  plot(density(y, kernel = "cosine", bw =m), col = "red", main = "Cosine K")
  plot(density(y, kernel = "rectangular", bw =m), col = "red", main = "Rect K")
  plot(density(y, kernel = "triangular", bw =m), col = "red", main = "Triangular K")
  plot(density(y, kernel = "epanechnikov", bw =m), col = "red", main = "epanechnikov K")
  plot(density(y, kernel = "biweight", bw =m), col = "red", main = "biweight K")
  
  
}

kfunction(Cars93$MPG.city)
mean(Cars93$Price)
ourmean(Cars93$Price)
removeOutliers(Cars93$Price)
mean(Cars93$MPG.city)
iqr=quantile(Cars93$MPG.city, 0.75)-quantile(Cars93$MPG.city, 0.25)
25+10.5
1.5*iqr
18-10.5

removeOutliers(Cars93$MPG.city)
