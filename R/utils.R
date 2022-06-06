#' Setup the Logging for Data Processing
#'
#' @param logfile create a log file for debugging. Can be one of `FALSE`
#' (default; do not create a file), `TRUE` (create the log file at the default
#' location), or a path for where to store the log file.
#' @param verbose Logical for if additional information should be displayed.
#' Defaults to `FALSE`.
#' @param outdir A directory the logfile will be saved
#'
#' @return Nothing.
setup_log <- function(logfile, verbose, outdir) {
  logger::log_layout(logger::layout_glue_colors)
  if (verbose) {
    logger::log_threshold("INFO")
    logger::log_errors()
  } else {
    logger::log_threshold("WARN")
  }

  if (isTRUE(logfile)) {
    # Logfile requested but location not specified
    logfile <- file.path(tempdir(), "gt3x_2_csv_log.log")
    logger::log_info("Created a log file at {crayon::blue(logfile)}")
  }

  if (is.character(logfile)) {
    logger::log_info("Creating a log file at {crayon::blue(logfile)}")

    if (file.exists(logfile)) {
      logger::log_info("Log file already exists. Will be replaced")
      unlink(logfile)
    }

    logger::log_threshold("TRACE", index = 2)
    logger::log_layout(logger::layout_simple, index = 2)
    logger::log_appender(logger::appender_file(logfile), index = 2)
    logger::log_success("Log file created")
    logger::log_errors()
  }
}


#' Check the Input Type for the Main Function
#'
#' Not intended to be called directly. Checks if the gt3x_files parameter is a
#' single file, a directory, or a vector of file paths. Errors if no GT3X files
#' are found.
#'
#' @param gt3x_files The parameter to be checked.
#'
#' @return One of "single", "directory", or "vector".
check_file_input <- function(gt3x_files) {
  # Check if single file, vector, or directory
  logger::log_trace("Checking what format gt3x_files is in")

  if (length(gt3x_files) == 1) {
    if (utils::file_test("-f", gt3x_files)) {
      proc_type <- "single"
      logger::log_trace("gt3x_files is a single file")
    } else if (utils::file_test("-d", gt3x_files)) {
      proc_type <- "directory"
      logger::log_trace("gt3x_files is a directory")
    } else {
      err <- "Could not find file or directory {crayon::blue(gt3x_files)}"
      stop(glue::glue(err))
    }
  } else {
    proc_type <- "vector"
    logger::log_trace("gt3x_files is a vector")
  }

  return(proc_type)
}

#' Check the GT3X Files are Valid
#'
#' Validates GT3X files using `read.gt3x::have_log_and_info`. Not intended to be
#' called by users.
#'
#' @param gt3x_files The file or files to check. Either a single file or a
#' vector of file paths.
#' @param proc_type The format of gt3x_files. Either "single", "directory", or
#' "vector".
#'
#' @return Nothing. Errors if invalid files are found.
validate_gt3x_files <- function(gt3x_files, proc_type) {
  logger::log_info("Validating files")
  # Validate the files
  if (proc_type == "single") {
    if (!read.gt3x::is_gt3x(gt3x_files) |
      !read.gt3x::have_log_and_info(gt3x_files)) {
      err <- "{crayon::blue(gt3x_files)} is not a valid gt3x file"
      stop(glue::glue(err))
    }
  }

  if (proc_type == "directory" | proc_type == "vector") {
    valid_files <- sapply(gt3x_files, read.gt3x::have_log_and_info)

    if (sum(valid_files) < length(gt3x_files)) {
      failed_files <- names(valid_files[valid_files == FALSE])
      err <- "Invalid file: {crayon::blue(failed_files)}"
      stop(glue::glue(err))
    }
  }
  logger::log_success("Files validated")
}
utils::globalVariables(c("failed_files"))


#' Convert a Directory to a Vector of Files
#'
#' Searches a directory for gt3x files. Not intended to be called by users.
#'
#' @param path The path the search for files.
#' @param recursive Should sub-folders also be searched? Defaults to TRUE.
#'
#' @return A vector of file paths.
list_gt3x_rec <- function(path, recursive = TRUE) {
  files <- list.files(path = path, full.names = TRUE, recursive = TRUE)
  return(files[read.gt3x::is_gt3x(files)])
}

#' Create File Names for Output Files
#'
#' Creates the vector of file paths that the resulting CSV files will be saved
#' to. Not intended to be called by users.
#'
#' @param gt3x_files The file or files to for which output locations will be
#' made.
#' @param outdir Optionally provide a single directory for all CSV files to be
#' stored in.
#'
#' @return A vector of file paths.
generate_outputfiles <- function(gt3x_files, outdir = NULL) {
  if (is.null(outdir)) {
    # Files are saved to the same place
    out_paths <- file.path(
      dirname(gt3x_files),
      gsub(".gt3x$", "RAW.csv", basename(gt3x_files),
        ignore.case = TRUE
      )
    )
  } else {
    # Files are saved to the outdir directory
    if (!utils::file_test("-d", outdir)) {
      err <- glue::glue("{crayon::blue(outdir)} is not a valid directory")
      stop(err)
    }
    out_paths <- file.path(
      outdir,
      gsub(".gt3x$", "RAW.csv", basename(gt3x_files),
        ignore.case = TRUE
      )
    )
  }
  return(out_paths)
}

impute_missing <- function(dt, missing, start_num, sample_rate) {
  if (is.null(missing)) {
    # Do nothing
    return(dt)
  }

  for (i in rev(seq_len(nrow(missing)))) {
    miss_row <- missing[i, ]

    if (miss_row[, "n_missing"] == 0) next

    miss_row_start <-
      (as.numeric(row.names(miss_row)) - start_num) * sample_rate

    miss_row_end <- miss_row_start + miss_row[, "n_missing"]
    vm <- sqrt(dt[miss_row_start, 1]^2 +
      dt[miss_row_start, 2]^2 +
      dt[miss_row_start, 3]^2)


    # TODO further validation of this eqn
    vm_thresh <- round((16.071 * (1 / sample_rate)) + 0.8669, 4)

    if (round(vm, 4) < vm_thresh) {
      data.table::set(
        dt,
        (miss_row_start + 1):miss_row_end, (1:3),
        dt[miss_row_start, ]
      )
    }
  }
  return(dt)
}