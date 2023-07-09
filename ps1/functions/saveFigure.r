# --- saveFigure --------------------------
#
# this function saves a figure in ggplot environment


saveFigure <- function(name,format,w,h,outputPath){
  
  name <- as.character(name)
  format <- as.character(format)

  ggsave(paste0(name,".",format), path = outputPath,
      width = w,
      height = h) 
  
  # return saving message
  return(print(paste0("Saved figure ", name,".",format, " in folder ",outputPath)))
}
