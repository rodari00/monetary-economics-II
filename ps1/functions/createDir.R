createDir <- function(country, folder_name){
  
  
# main (parent)
results.dir <- paste0(getwd(),"/_results")


# figures (child)
figures.dir <- paste0(results.dir,
                      "/figures/", country, 
                      "/", folder_name)

dir.create(figures.dir,
            showWarnings = FALSE, recursive = TRUE)
  
message(paste0('Created figures.dir at: ', figures.dir))  

# data (child)
data.dir <- paste0(results.dir,
                   "/data/", country, 
                   "/", folder_name)
  

dir.create(data.dir,
            showWarnings = FALSE, recursive = TRUE)  
  
message(paste0('Created data.dir at: ', data.dir)) 

return(list(figures.dir,data.dir))
  
}