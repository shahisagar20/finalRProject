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
  if(class(x) == 'numeric' & length(x) != 0){
    for(i in x){
      if(!i.isNa){
        y[j] = i
        j =j+1
      }
    }
    first_quartile = quantile(x, 0.25)
    third_quartile = quantile(x, 0.75)
    Inner_Quartile_Range = third_quartile - first_quartile
    leveler = 1.5*Inner_Quartile_Range
    minleveler = first_quartile - leveler
    maxleveler = third_quartile + leveler
    
    output = NULL
    for(i in 1:length(x)){
      if(x[i]> minleveler & x[i] < maxleveler ){
        output[i] = x[i]
      }
    }
    return(output)
  }else{
    print("The input data should be numeric. ")
    }
}

ourmean = function(x){
  y = removeOutliers(x)
  
}
