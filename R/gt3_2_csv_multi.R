
#' gt3x_folder_2_csv
#' 
#' Converts al the files inside a folder
#' 
#' Lists all the .gt3x files in a given folder and converts it to the actilife RAW data csv file
#' @param folder_path The folder where the files are located
#' @export
#' @import tictoc
#' @return A folder named "csv" inside the folder where the .gt3x files are located
#' @seealso gt3x_2_csv
#' @seealso gt3x_2_csv_par

gt3x_folder_2_csv <- function(folder_path) {
  
  file_names <- list.files(folder_path,
                          pattern = ".gt3x",
                          full.names = TRUE)
  
  
  tic(paste("Finished processing all ", length(file_names), " files"))
  
  for (i in 1:length(file_names)) {
    
    gt3x_2_csv(file_names[i])
    
  }
  
  unlink(paste0(folder_path,"/unzip",
                recursive = TRUE))
  
  toc()
  
}
