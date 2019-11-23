
library("read.gt3x")
library("tidyverse")
library("data.table")
library("tictoc")

#' @title substrRight
#' 
#' @description Extracting n characters from the end of the string "jumping" last j characters
#' @param x the string to be manipulated
#' @param n the number of characters to extract from the string
#' @param j the number of characters to ignore in the end of the string

substrRight <- function(x, n = nchar(x)- j, j = 0){
   substr(x, nchar(x)-(n+j)+1, nchar(x)-j)
}



  #' @title transform_dates
  #' 
  #' @description Changing the formatting of the date-time information
  #' 
  #' @details Util to change the format of the date-times to the format used in the .gt3x file
  #' @param x the object containing the date to be changed.
  
  transform_dates <- function(x) {
     as.POSIXct(x, origin = "0001-01-01", tz = "UTC")
  }
  
  
  #' @title divide_1e7 
  #' 
  #' @description util function to the right format of dates
  #' @param y value to be divided by 1e7
  
  divide_1e7 <- function(y) {
     y / 1e7
  }
  
   
  #### READ_INFO function
  
  #' @title Read Info 
  #' 
  #' @description Reads the metadata of the gt3x file 
  #' 
  #' @deteails Reads the metadata registered in the .txt file that is contained inside .gt3x file provided by the actilife software
  #' @param file_txt The path to the desired .txt file
  #' @import tidyverse
  
  read_info <- function(file_txt = file_txt) {
     
     info_file <- data.frame(key = readLines(file_txt)) %>%
        separate(key, c("key", "value"), ": ") %>%
        spread(key, value)
     
    info_file %>%
        rename_all(funs(make.names((names(info_file))))) %>%
        mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), as.numeric) %>%
        mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), divide_1e7) %>%
        mutate_at(vars( Download.Date, Last.Sample.Time, Start.Date, Stop.Date), transform_dates)
     
  }
  
  
  
  #### SAVE HEADER FUNCTION
  ## Saves header in the same format as the Actilife RAW csv output.
  
  #' @title save_header
  #' 
  #' @description Saves .gt3x metadata as csv header
  #'
  #' @details Saves the header extracted from the .gt3x file with the read_info function in the .csv extension (look at read_info function)
  #' @param infofile default = info_filef data frame containing the metadata generated through the read_info function
  #' @param dest_csv  default = ddestination folder. the folder to which you want to generate the header file
  #' @param files_list_i the name of the file that is going to be saved
  #' @import hms
  #' @import lubridate
  
  save_header <- function(df_file = info_filef, dest_csv = csv_folder, file_id)
  {
     #formatting the metadata to the actilife header form
     
     header_txt <- paste0( "------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 Firmware v1.9.2 date format dd/MM/yyyy at 30 Hz  Filter Normal -----------\n",
                           "Serial Number: ", df_file$Serial.Number, "\n",
                           "Start Time ", as_hms(df_file$Start.Date), "\n",
                           "Start Date ", format(df_file$Start.Date, "%d/%m/%Y"), "\n",
                           "Epoch Period (hh:mm:ss) 00:00:00\n",
                           "Download Time ", as_hms(df_file$Download.Date), "\n",
                           "Download Date ", format(df_file$Download.Date, "%d/%m/%Y"), "\n",
                           "Current Memory Address: 0\n",
                           "Current Battery Voltage: ", sub(",", ".", df_file$Battery.Voltage),"     Mode = 12\n",
                           "--------------------------------------------------\n")
 
     # Writing the .csv document with the header
    
     cat(header_txt,
         file = paste0( dest_csv, "/", file_id, "RAW.csv"))
  }
  

  #' @title header_csv
  #' 
  #' @description Reads the metadata from gt3x files and savaes as csv
  #' 
  #' @details Reads the metadata from the txt file located inside the .gt3x file provided by actigraph using the read_info function and saves it as a csv document using the save_header function.
  #' @param origin the path to the .gt3xfile to be converted
  #' @param dest default = same directory of the data.  the destination were the .csv file is going to be placed (to be implemented)
  #' @import read.gt3x
  #' @import tidyverse
  
  header_csv <- function( origin ) {
  
    dest <- substrRight( origin, j = 13)
     
    #file name 
    
    file_id <- substrRight( origin, 7, 5)
    
    print( file_id)
    
     # Results directory
     
     csv_folder <- paste0( dest, "/csv")
     
     if ( dir.exists( csv_folder) == FALSE ) {
     
     dir.create( csv_folder)
      
     message( "csv output folder created as: ", csv_folder) 
        
     } else {
          
       message( "csv folder already exists")
       
        }
     
     # Unzipping the file 
     
     unzip_single_gt3x( origin, location = paste0( dest, "/unzip"), verbose = FALSE)
     
     # Creating the path to the desired .txt file
     
     # Unzipped folder address
     
     unzipath <- paste0( dest, "/unzip")
     
    
     # Unzipped folder address
    
     
     unzip_folder_addres <- paste0( unzipath, "/", file_id)
     
     # Txt address
     
     file_txt <- paste0( unzip_folder_addres, "/info.txt")
     
     info_filef <- read_info( file_txt)
     
     save_header( df_file = info_filef, dest_csv = csv_folder, file_id = file_id)
     
     message("Header saved as:  ", csv_folder, "/", file_id, "RAW.csv")
     }
 
  ## Saves acceleration data in the same format as Actilife RAW csv output
  
  #' @title save_accel
  #' 
  #' @description  Saves acceleration of the given file 
  #' 
  #' @details Reads the binary data inside the .gt3x file and saves it in .csv format
  #' @param acc.file the path to te .gt3x file 
  #' @import read.gt3x
  #' @import tidyverse
  #' @import tictoc
  
save_accel <- function( acc.file ) {
  
  #file name 
  
  file_id <- substrRight( acc.file, 7, 5)
  
  message ( "Reading acceleration", file_id)
  # Reading acceleration
  
  accel <- read.gt3x( acc.file,
                      imputeZeroes = TRUE)
  
  accel_df <- as.data.frame( accel[ ,-4])
  
  accel_df$X <- as.character( accel_df$X )
  accel_df$Y <- as.character( accel_df$Y )
  accel_df$Z <- as.character( accel_df$Z )
  
  names( accel_df ) <- c( "Accelerometer X",
                        "Accelerometer Y",
                        "Accelerometer Z")
  
  # Extracting the folder path
  
  dest <- substrRight( acc.file, j = 13 )
  
  # Results directory
  
  csv_folder <- paste0( dest, "/csv")
  
  # Writing acceleration data in csv:
  
  message( "Writing acceleration", file_id)
  
  tic( "Acceleration written ")
  fwrite( x = accel_df,
          file = paste0( csv_folder, "/", file_id, "RAW.csv"), 
          append = TRUE,
          sep = ",",
          col.names = TRUE,
          row.names = FALSE )
  
  toc()
  
}


## FUNCTION THAT CONVERTS A GT3X FILE IN CSV FILE
# Path refers to the folder where the GT3X files are stored

#' @title gt3x_2_csv
#' 
#' @description Converts a given .gt3x file to .csv format
#' 
#' @details Reads both the .txt file and the .bin file located inside the .gt3x file given by actilife software and converts it to a csv file in the save format of the .csv file extracted from the sofrtware.
#' @param path the path to the given file 
#' @param dest_csv desired destination folder
#' @import read.gt3x
#' @import tidyverse
#' @import data.table
#' @import lubridate
#' @import hms
#' 
gt3x_2_csv <- function( gt3x_file )
  
{
  print( "Started processing file" )
  
  file_id <- substrRight( gt3x_file, 7, 5)
  
  tic( paste( "File number", i ,"named", file_id, " processed" ) )
      
  header_csv( gt3x_file )
  
  save_accel( gt3x_file )
  
  dest <- substrRight( gt3x_file, j = 13 )
  
  file_id <- substrRight( gt3x_file, 7, 5 )
  
  unzipath <- paste0( dest, "/unzip")
  
  
  # Unzipped folder address
  
  
  unzip_folder_addres <- paste0( unzipath,"/", file_id )
  
  unlink( unzip_folder_addres, recursive = TRUE )
  
  toc()
}  

