#' Convert GT3X Files to Raw CSV
#'
#' TODO ADD DESCRIPTION
#'
#' @param gt3x_files The file or files to convert. You can provide any of a
#' path to a single file, a path to a directory, or a vector of file paths.
#' @param outdir A directory where converted CSV files will be saved. If NULL,
#' the files are saved in the same directory as the original files.
#' @param progress Display a progress bar. Defaults to TRUE.
#' @param parallel Use a parallel backend. Defaults to TRUE.
#' @param cores If `parallel == TRUE`, how many cores are used for processing.
#' By default this is the smaller of the number of cores available minus 1, or
#' the number of files to process.
#' @param logfile create a log file for debugging. Can be one of `FALSE`
#' (default; do not create a file), `TRUE` (create the log file at the default
#' location), or a path for where to store the log file.
#' @param verbose Logical for if additional information should be displayed.
#' Defaults to `FALSE`
#' @param recursive If gt3x_files is a directory, should sub-folders be searched
#' for GT3X  files?
#' @param actilife The version string for the header. By default, this is
#' "gt3x2csv v0.2.0". If your analysis depends on knowing an Actilife version,
#' you can provide one here.
#'
#' @importFrom foreach %dopar%
#'
#' @return Nothing. Files are saved as a side effect.
#' @export
#'
#' @examples
gt3x_2_csv <- function(gt3x_files,
                       outdir = NULL,
                       progress = FALSE,
                       parallel = TRUE,
                       cores = NULL,
                       logfile = FALSE,
                       verbose = FALSE,
                       recursive = TRUE,
                       actilife = FALSE) {
  setup_log(logfile, verbose, outdir)

  proc_type <- check_file_input(gt3x_files)

  # If a directory, convert to a list of files
  if (proc_type == "directory") {
    logger::log_trace("Converting directory to a vector of files")
    gt3x_files <- list_gt3x_rec(gt3x_files, recursive)
    num_files <- length(gt3x_files)
    msg <- "Found {crayon::blue(num_files)} GT3X files"

    if (num_files < 1) {
      stop(msg)
    }
    logger::log_info(msg)
  }

  validate_gt3x_files(gt3x_files, proc_type)

  outfiles <- generate_outputfiles(gt3x_files, outdir)

  # Setup processing backend
  if (is.null(cores)){
    cores <- min(length(gt3x_files), parallel::detectCores() - 1)
  }

  if (!parallel | proc_type == "single") {
    logger::log_info("Setting up a sequential backend")
    foreach::registerDoSEQ()
  } else {
    logger::log_info(
      "Setting up a parallel backend with {crayon::blue(cores)} cores")
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
  }

  # Setup progress bar
  if (progress) {
    bar <- switch(.Platform$OS.type,
      windows = {
        utils::winProgressBar(
          title = "Converting GT3X files to CSV. Progress:",
          min = 0,
          max = length(gt3x_files),
          label = "0% done",
          width = 500
        )
      },
      unix = {
        tcltk::tkProgressBar(
          title = "Converting GT3X files to CSV. Progress:",
          min = 0,
          max = length(gt3x_files),
          label = "0% done",
          width = 500
        )
      }
    )

    update_progress <- function(n) {
      switch(.Platform$OS.type,
        windows = {
          utils::setWinProgressBar(bar,
            n,
            label = paste0(round(n / length(gt3x_files) * 100, 0), "% done")
          )
        },
        unix = {
          tcltk::setTkProgressBar(bar,
            n,
            label = paste0(round(n / length(gt3x_files) * 100, 0), "% done")
          )
        }
      )
    }

    opts <- list(progress = update_progress)
  } else {
    opts <- list()
  }

  # Process the files
  i <- 1
  start_loop <- Sys.time()
  foreach::foreach(
    i = seq_len(length(gt3x_files)),
    .inorder = FALSE,
    .export = c("convert_file", "save_header", "save_accel"),
    .options.snow = opts
  ) %dopar% {
    convert_file(gt3x_files[i], outfiles[i])
  }

  if (parallel & proc_type != "single") parallel::stopCluster(cl)

  if (progress) {
    close(bar)
  }
  loop_comp <- difftime(Sys.time(),start_loop)
  loop_comp_time <- round(as.numeric(loop_comp), 2)
  loop_comp_units <- attr(loop_comp, "units")
  msg <- glue::glue("Finished processing {crayon::blue(length(gt3x_files))} ",
                    "files in {crayon::blue(loop_comp_time, loop_comp_units)}")
  logger::log_success(msg)

}

#' Convert a Single GT3X File
#'
#' Not intended to be called directly. The function to drive the foreach loop.
#'
#' @param gt3x_file A path to a single GT3X file
#' @param outfile The path to save the resulting CSV file
#' @param actilife The version string for the header. By default, this is
#' "gt3x2csv v0.2.0". If your analysis depends on knowing an Actilife version,
#' you can provide one here.
#'
#' @return nothing
convert_file <- function(gt3x_file, outfile, actilife = FALSE) {
  msg <- glue::glue("Starting conversion.
                    Input: {crayon::blue(gt3x_file)}
                    Output: {crayon::blue(outfile)}")

  logger::log_trace(msg)
  logger::log_info()
  file_start <- Sys.time()
  gt3x_file_read <- read.gt3x::read.gt3x(gt3x_file)
  gt3x2csv:::save_header(gt3x_file_read, outfile, actilife)
  gt3x2csv:::save_accel(gt3x_file_read, outfile)

  comp_time <- round(
    as.numeric(difftime(Sys.time(),
      file_start,
      units = "secs"
    )),
    2
  )

  fsize <- format(structure(file.size(gt3x_file),
    class = "object_size"
  ), units = "auto", standard = "SI")

  msg <- glue::glue(
    "Completed conversion of {crayon::blue(gt3x_file)} in ",
    "{crayon::blue(comp_time)} seconds. ",
    "Approx file size: {crayon::blue(fsize)}."
  )
  logger::log_success(msg)
}


#' Save Information to CSV Header
#'
#' Reads information from the 'info.txt' file and writes it in the expected
#' format to a CSV file. Not intended to be called directly.
#'
#' @param gt3x_file An object read using `read.gt3x::read.gt3x()`
#' @param outfile The path to save the resulting CSV file
#' @param actilife The version string for the header. By default, this is
#' "gt3x2csv v0.2.0". If your analysis depends on knowing an Actilife version,
#' you can provide one here.
#'
#' @return nothing
save_header <- function(gt3x_file,
                        outfile,
                        actilife = FALSE) {
  # Header objects ---------------------------------
  if (!is.character(actilife)) {
    actilife <- paste("gt3x2csv",
      getNamespaceVersion("gt3x2csv"),
      collapse = " "
    )
  }
  logger::log_trace("Getting header information")
  header <- attr(gt3x_file, "header")
  start_date_unfmt <- header$`Start Date`
  dwnld_date_unfmt <- header$`Download Date`

  sample_rate <- attr(gt3x_file, "sample_rate")
  serial_num <- header$`Serial Number`
  start_time <- format(start_date_unfmt, format = "%H:%M:%S")
  start_date <- format(start_date_unfmt, "%d/%m/%Y")
  dwnld_time <- format(dwnld_date_unfmt, format = "%H:%M:%S")
  dwnld_date <- format(dwnld_date_unfmt, "%d/%m/%Y")
  batt_volt <- gsub(",","", header$`Battery Voltage`)
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
      "Current Battery Voltage: {batt_volt}     Mode = 12\n",
      "--------------------------------------------------\n\n"
    )
  # nolint end
  logger::log_trace("Writing header to CSV")
  cat(header_txt, file = outfile)
}
utils::globalVariables(c("sample_rate", "serial_num", "start_time",
                         "start_date", "dwnld_time", "dwnld_date", "batt_volt",
                         "firmware"))

#' Write Activity Data to CSV
#'
#' Write the activity data to a CSV file. Not intended to be called directly.
#'
#' @param gt3x_file An object read using `read.gt3x::read.gt3x()`
#' @param outfile The path to save the resulting CSV file
#'
#' @return Nothing
save_accel <- function(gt3x_file, outfile) {
  outdata <- data.table::as.data.table(gt3x_file)
  outdata <- setNames(outdata, c("Accelerometer X",
                                 "Accelerometer Y",
                                 "Accelerometer Z"))

  logger::log_trace("Writing activity to CSV")
  vroom::vroom_write(outdata,
    file = outfile,
    append = TRUE,
    delim = ",",
    col_names = TRUE
  )
}
