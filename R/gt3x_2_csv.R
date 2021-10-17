#' Convert GT3X Files to Raw CSV
#'
#' TODO ADD DESCRIPTION
#'
#' @param gt3x_files the file or files to convert. You can provide any of a
#' path to a single file, a path to a directory, or a vector of file paths.
#' @param outdir a directory where converted CSV files will be saved. If NULL,
#' the files are saved in the same directory as the original files. If
#' gt3x_files is a directory and `recursive==TRUE`, the folder structure is
#' replicated in outdir.
#' @param progress Display a progress bar. Defaults to TRUE.
#' @param parallel Use a parallel backend. Defaults to TRUE.
#' @param cores If `parallel == TRUE`, how many cores are used for processing.
#' @param logfile create a log file for debugging. Can be one of `FALSE`
#' (default; do not create a file), `TRUE` (create the log file at the default
#' location), or a path for where to store the log file.
#' @param verbose logical for if additional information should be displayed.
#' Defaults to `FALSE`
#' @param recursive if gt3x_files is a directory, should sub-folders be searched
#' for GT3X  files?
#'
#' @importFrom foreach %dopar%
#'
#' @return
#' @export
#'
#' @examples
gt3x_2_csv <- function(gt3x_files,
                       outdir = NULL,
                       progress = FALSE,
                       parallel = TRUE,
                       cores = parallel::detectCores() - 1,
                       logfile = FALSE,
                       verbose = FALSE,
                       recursive = TRUE) {
  # TODO sort out a system for outdir
  setup_log(logfile, verbose, outdir)

  proc_type <- check_file_input(gt3x_files)

  # If a directory, convert to a list of files
  if (proc_type == "directory") {
    logger::log_trace("Converting directory to a vector of files")
    gt3x_files <- list_gt3x_rec(gt3x_files, recursive)
    num_files <- length(gt3x_files)
    msg <- "Found {crayon::blue(num_files)} gt3x files"

    if (num_files < 1) {
      stop(msg)
    }

    logger::log_info(msg)

  }

  validate_gt3x_files(gt3x_files, proc_type)

  # Setup processing backend
  if (!parallel | proc_type=="single") {
    logger::log_info("Setting up a sequential backend")
    foreach::registerDoSEQ()
  } else {
    logger::log_info("Setting up a parallel backend with {cores} cores")
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
  }

  #TODO progress bar




  parallel::stopCluster(cl)

}

save_header <- function(gt3x_file,
                        outfile,
                        verbose = FALSE,
                        actilife = FALSE) {
  # Header objects ---------------------------------
  if (!is.character(actilife)) {
    actilife <- paste("gt3x2csv",
      getNamespaceVersion("gt3x2csv"),
      collapse = " "
    )
  }
  header <- attr(gt3x_file, "header")
  start_date_unfmt <- header$`Start Date`
  dwnld_date_unfmt <- header$`Download Date`

  sample_rate <- attr(gt3x_file, "sample_rate")
  serial_num <- header$`Serial Number`
  start_time <- format(start_date_unfmt, format = "%H:%M:%S")
  start_date <- format(start_date_unfmt, "%d/%m/%Y")
  dwnld_time <- format(dwnld_date_unfmt, format = "%H:%M:%S")
  dwnld_date <- format(dwnld_date_unfmt, "%d/%m/%Y")
  batt_volt <- header$`Battery Voltage`
  firmware <- header$Firmware

  # Header Text -------------------------------------
  header_txt <-
    # nolint start
    glue::glue(
      "------------ Data File Created By ActiGraph GT3X+ {actilife} Firmware v{firmware} date format dd/MM/yyyy at {sample_rate} Hz  Filter Normal -----------\n",
      "Serial Number: {serial_num}\n",
      "Start Time {start_time}\n",
      "Start Date {start_date}\n",
      "Epoch Period (hh:mm:ss) 00:00:00\n",
      "Download Time {dwnld_time}\n",
      "Download Date {dwnld_date}\n",
      "Current Memory Address: 0\n",
      "Current Battery Voltage: {batt_volt}\n",
      "--------------------------------------------------\n\n"
    )
  # nolint end

  cat(header_txt, file = outfile)
}

save_accel <- function(gt3x_file, outfile, verbose = FALSE) {
  vroom::vroom_write(data.table::as.data.table(gt3x_file),
    file = outfile,
    append = TRUE,
    delim = ",",
    col_names = TRUE
  )
}
