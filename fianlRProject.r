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
 if(class(x)=='integer'){
   x = as.numeric(x)
 }
  if(class(x) == 'numeric' & length(x) != 0){
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
    print("The input data should be numeric. ")
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


ourmean(x)

#Function to produce the kernel density plots
kfunction = function(x){
  out = removeOutliers(x)
  m = ourmean(y)
  
  y = 0
  j=1
  for(i in out){
    if(!is.na(i)){
      y[j] = i
      j =j+1
    }
  }
  par(mfrow =c(2,3))
  plot(density(y, kernel = "gaussian", bw = m), col = "red", main = "Gaussian K")
  plot(density(y, kernel = "cosine", bw =m), col = "red", main = "Cosine K")
  plot(density(y, kernel = "rectangular", bw =m), col = "red", main = "Rect K")
  plot(density(y, kernel = "triangular", bw =m), col = "red", main = "Triangular K")
  plot(density(y, kernel = "epanechnikov", bw =m), col = "red", main = "epanechnikov K")
  plot(density(y, kernel = "biweight", bw =m), col = "red", main = "biweight K")
  
  
}
class(Cars93$Max.Price)
kfunction(Cars93$MPG.city)
class(x)
class(as.numeric(Cars93$MPG.city))
kfunction(Cars93$Max.Price)
