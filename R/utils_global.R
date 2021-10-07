##  Setting global variables
#'  
#' @keywords internal
#' 
#' @importFrom utils globalVariables
#' 
globalVariables(c("Download.Date", "Last.Sample.Time", "Start.Date", "Stop.Date",
                  "csv_folder", "file_id", "i", "info_filedf", "value",
                  "key", ".", "accel", "X", "Y", "Z", "it_file"))

##  Importing pipe operators
#'  
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang :=
NULL