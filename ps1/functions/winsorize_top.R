winsorize_top = function(x, cut = 0.05){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  
  
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top

  return(x)
}
