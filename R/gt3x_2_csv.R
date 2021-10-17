gt3x_2_csv <- function(gt3x_files,
                       outdir,
                       progress = FALSE,
                       parallel = TRUE,
                       cores = parallel::detectCores() - 1,
                       logfile = FALSE,
                       verbose = FALSE,
                       recursive = TRUE) {
  setup_log(logfile, verbose)

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

  test_vec <- c(file.path(gt3x_files, "EE_left_29.5.2017-05-30.gt3x"),
                file.path(gt3x_files, "SS_left_19.5.2017-05-22.gt3x"),
                file.path(gt3x_files, "gt3x_2_csv_log.gt3x"))

  test_dir <- "C:\\Users\\taren\\Downloads\\Test gt3x Files"

  test_dir_files <- list_gt3x_rec(test_dir)
  read.gt3x::have_log_and_info(test_dir_files)

  valid_files <- sapply(test_vec, read.gt3x::have_log_and_info)

  valid_files[!valid_files]

  # If single file, skip the parallel stuff

  # If multiple files, setup the parallel options

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
