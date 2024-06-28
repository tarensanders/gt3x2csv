#' Convert agd Files to CSV
#'
#' Convert a agd file to the CSV format provided by ActiLife.
#'
#' @param agd_files The file or files to convert. You can provide any of a
#' path to a single file, a path to a directory, or a vector of file paths.
#' @param outdir A directory where converted CSV files will be saved. If NULL,
#' the files are saved in the same directory as the original files.
#' @param vm Should the vector magnitude column be included? Defaults to TRUE.
#' @param progress Display a progress bar. Defaults to TRUE.
#' @param parallel Use a parallel backend. Defaults to FALSE.
#' @param cores If `parallel == TRUE`, how many cores are used for processing.
#' By default this is the smaller of the number of cores available minus 1, or
#' the number of files to process.
#' @param logfile create a log file for debugging. Can be one of `FALSE`
#' (default; do not create a file), `TRUE` (create the log file at the default
#' location), or a path for where to store the log file.
#' @param verbose Logical for if additional information should be displayed.
#' Defaults to `FALSE`
#' @param recursive If agd_files is a directory, should
#' sub-folders be searched for agd files?
#'
#' @importFrom foreach %dopar%
#'
#' @return Nothing. Files are saved as a side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' agd_2_csv(
#'   agd_files = my_directory,
#'   outdir = NULL, # Save to the same place
#'   progress = FALSE, # Show a progress bar?
#'   parallel = TRUE # Process files in parallel?
#' )
#' }
agd_2_csv <- function(
    agd_files, outdir = NULL, vm = TRUE, progress = FALSE, parallel = TRUE,
    cores = NULL, logfile = FALSE, verbose = FALSE, recursive = TRUE) {
  start_proc_time <- Sys.time()
  setup_log(logfile, verbose, outdir)

  proc_type <- check_file_input(agd_files)

  if (proc_type == "directory") {
    logger::log_trace("Converting directory to vector of files")
    agd_files <- list_agd_rec(agd_files, recursive)
    num_files <- length(agd_files)
    msg <- glue::glue("Found {num_files} agd files")
    if (num_files < 1) {
      stop(msg)
    }
    logger::log_info(msg)
  }

  outfiles <- generate_outputfiles(agd_files, outdir, type = "agd", suffix = ".csv")
  if (is.null(cores)) {
    cores <- min(length(agd_files), parallel::detectCores() - 1)
  }
  if (!parallel || proc_type == "single") {
    logger::log_info("Setting up a sequential backend")
    foreach::registerDoSEQ()
  } else {
    logger::log_info(
      "Setting up a parallel backend with {crayon::blue(cores)} cores"
    )
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
  }

  # Setup progress bar
  if (progress) {
    bar <- switch(.Platform$OS.type,
      windows = {
        utils::winProgressBar(
          title = "Converting agd files to CSV. Progress:",
          min = 0,
          max = length(agd_files),
          label = "0% done",
          width = 500
        )
      },
      unix = {
        tcltk::tkProgressBar(
          title = "Converting agd files to CSV. Progress:",
          min = 0,
          max = length(agd_files),
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
            label = paste0(round(n / length(agd_files) * 100, 0), "% done")
          )
        },
        unix = {
          tcltk::setTkProgressBar(bar,
            n,
            label = paste0(round(n / length(agd_files) * 100, 0), "% done")
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
  foreach::foreach(
    i = seq_len(length(agd_files)),
    .inorder = FALSE,
    .export = c("convert_agd_file", "save_agd_header", "save_agd_data"),
    .options.snow = opts
  ) %dopar% {
    convert_agd_file(agd_files[i], outfiles[i], cores = cores, vm = vm)
  }

  if (parallel && proc_type != "single") parallel::stopCluster(cl)

  if (progress) {
    close(bar)
  }
  comp <- difftime(Sys.time(), start_proc_time)
  comp_time <- round(as.numeric(comp), 2)
  comp_units <- attr(comp, "units")
  msg <- glue::glue(
    "Finished processing {crayon::blue(length(agd_files))} ",
    "files in {crayon::blue(comp_time, comp_units)}"
  )
  logger::log_success(msg)
}


#' Convert a Single agd File
#'
#' Not intended to be called directly. The function to drive the foreach loop.
#'
#' @param agd_file A path to a single agd file
#' @param outfile The path to save the resulting CSV file
#' @param cores The number of cores to use for parallel processing.
#' @param vm Should the vector magnitude column be included?
#' @param dateformat The format to use for the date string in the header.
#' @param include_timestamp Should the timestamp column be included in the
#' output?
#'
#' @return nothing
convert_agd_file <- function(
    agd_file, outfile, cores, vm, dateformat = "%Y-%m-%d",
    include_timestamp = FALSE) {
  msg <- glue::glue("Starting conversion.
                    Input: {crayon::blue(agd_file)}
                    Output: {crayon::blue(outfile)}")
  logger::log_trace(msg)
  file_start <- Sys.time()

  con <- DBI::dbConnect(RSQLite::SQLite(), agd_file)
  save_agd_header(con, outfile, dateformat)
  save_agd_data(con, outfile, cores, include_timestamp, vm)
  DBI::dbDisconnect(con)

  comp_time <- round(
    as.numeric(difftime(Sys.time(), file_start, units = "secs")), 2
  )
  msg <- glue::glue(
    "Completed conversion of {crayon::blue(agd_file)} in ",
    "{crayon::blue(comp_time)} seconds. "
  )
  logger::log_success(msg)
}

#' Save Information to CSV Header
#'
#' Reads information from agd file and writes it in the expected format to a CSV
#' file. Not intended to be called directly.
#'
#' @param con The connection to the agd file established by `DBI::dbConnect()`
#' @param outfile The path to save the resulting CSV file
#' @param dateformat The format to use for the date string in the header.
#'
#' @return nothing
save_agd_header <- function(con, outfile, dateformat) {
  get_setting_value <- function(setting_name) {
    settings_table[
      settings_table["settingName"] == setting_name,
      "settingValue"
    ]
  }
  format_datetime <- function(datetime) {
    as.POSIXlt(
      (as.numeric(datetime) / 1e7),
      origin = "0001-01-01 00:00:00", tz = "UTC"
    )
  }
  convert_date_format <- function(format_string) {
    replacements <- c("%Y" = "yyyy", "%m" = "mm", "%d" = "dd")
    for (pattern in names(replacements)) {
      format_string <- gsub(pattern, replacements[pattern], format_string)
    }
    format_string
  }
  settings_table <- DBI::dbReadTable(con, "settings")

  start_datetime <- format_datetime(get_setting_value("startdatetime"))
  download_datetime <- format_datetime(get_setting_value("downloaddatetime"))

  header_data <- list(
    device = get_setting_value("devicename"),
    software = get_setting_value("softwarename"),
    software_version = get_setting_value("softwareversion"),
    firmware = get_setting_value("deviceversion"),
    dateformat = convert_date_format(dateformat),
    filter = get_setting_value("filter"),
    serial_num = get_setting_value("deviceserial"),
    start_time = strftime(start_datetime, "%H:%M:%S"),
    start_date = format(start_datetime, dateformat),
    epoch = strftime(
      as.POSIXlt(as.numeric(get_setting_value("epochlength")),
        origin = "0001-01-01 00:00:00", tz = "UTC"
      ),
      format = "%H:%M:%S"
    ),
    download_time = strftime(download_datetime, "%H:%M:%S"),
    download_date = format(download_datetime, dateformat),
    batt_volt = get_setting_value("batteryvoltage"),
    mode = get_setting_value("modenumber")
  )

  header_txt <-
    # nolint start
    glue::glue(
      "------------ Data File Created By ActiGraph {header_data$device} {header_data$software} v{header_data$software_version} Firmware v{header_data$firmware} date format {header_data$dateformat} Filter {header_data$filter} -----------\n",
      "Serial Number: {header_data$serial_num}\n",
      "Start Time {header_data$start_time}\n",
      "Start Date {header_data$start_date}\n",
      "Epoch Period (hh:mm:ss) {header_data$epoch}\n",
      "Download Time {header_data$download_time}\n",
      "Download Date {header_data$download_date}\n",
      "Current Memory Address: 0\n",
      "Current Battery Voltage: {header_data$batt_volt}     Mode = {header_data$mode}\n",
      "--------------------------------------------------\n\n"
    )
  # nolint end

  logger::log_trace("Writing header to CSV")
  cat(header_txt, file = outfile)
}

#' Write Activity Data to CSV
#'
#' Write the activity data to a CSV file. Not intended to be called directly.
#'
#' @param con The connection to the agd file established by `DBI::dbConnect()`
#' @param outfile The path to save the resulting CSV file
#' @param cores The number of cores allocated to parallel processing
#' @param include_timestamp Should the timestamp column be included in the
#' output?
#' @param vm Should the vector magnitude column be included?
#'
#' @return Nothing
save_agd_data <- function(con, outfile, cores, include_timestamp, vm) {
  # TODO: Option to include the timestamp column in the output
  logger::log_trace("Extracting data")
  data_table <- DBI::dbReadTable(con, "data")

  axis_cols <- c("axis1", "axis2", "axis3")
  axis_col_names <- c("Axis1", "Axis2", "Axis3")

  axis_cols_exist <- intersect(axis_cols, colnames(data_table))
  axis_data <- data_table[axis_cols_exist]

  new_names <- axis_col_names[match(axis_cols_exist, axis_cols)]

  colnames(axis_data) <- new_names

  if (vm) {
    logger::log_trace("Calculating vector magnitude")
    axis_data$vm <- sqrt(rowSums(axis_data^2))
  }

  # Write the data to the outfile
  logger::log_trace("Writing activity to CSV")
  vroom::vroom_write(axis_data,
    file = outfile, append = TRUE, delim = ",", col_names = TRUE,
    num_threads = max(1, parallel::detectCores() - cores - 1)
  )
}
