setup_log <- function(logfile, verbose, outdir) {
  # Set up logging
  logger::log_layout(logger::layout_glue_colors)
  if (verbose) {
    logger::log_threshold("INFO")
  } else {
    logger::log_threshold("FATAL")
  }

  if (isTRUE(logfile)) {
    # Logfile requested but location not specified
    logfile <- file.path(outdir, "gt3x_2_csv_log.log")
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
  }

  logger::log_errors()
}


check_file_input <- function(gt3x_files) {
  # Check if single file, vector, or directory
  logger::log_trace("Checking what format gt3x_files is in")

  if (length(gt3x_files) == 1) {
    if (utils::file_test("-f", gt3x_files)) {
      proc_type <- "single"
      logger::log_trace("gt3x_files is a single file")
    } else if (utils::file_test("-d", gt3x_files)) {
      proc_type <- "dir"
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


list_gt3x_rec <- function(path, recursive = TRUE) {
  files <- list.files(path = path, full.names = TRUE, recursive = TRUE)
  return(files[read.gt3x::is_gt3x(files)])
}

generate_outputfiles <- function(gt3x_files, outdir) {
  if (is.null(outdir)) {
    # Files are saved to the same place
    out_paths <- file.path(
      dirname(test_vec),
      gsub(".gt3x$", "RAW.csv", basename(test_vec),
        ignore.case = TRUE
      )
    )
  } else {
    # Files are saved to the outdir directory
    if (!utils::file_test("-d", outdir)) {
      err <- "{crayon::blue(outdir)} is not a valid directory"
      stop(err)
    }
    out_paths <- file.path(
      outdir,
      gsub(".gt3x$", "RAW.csv", basename(test_vec),
        ignore.case = TRUE
      )
    )
  }
  return(out_paths)
}
