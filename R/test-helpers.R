local_dir_with_files <- function(dir = tempfile(),
                                 num_files = 5,
                                 inc_good = FALSE,
                                 env = parent.frame()) {
  dir.create(dir)

  withr::defer({unlink(dir, recursive = TRUE)}, envir = env)


  # Add example files
  if (num_files > 0) {
    for (i in seq_len(num_files)) {
      file.copy(
        testthat::test_path("examples", "test_file.gt3x"),
        file.path(dir, paste0("test_file", i, ".gt3x"))
      )
    }
  }

  # Add 'known good' file
  if (inc_good) {
    file.copy(
      testthat::test_path("examples", "actilife_file.csv"),
      file.path(dir, "actilife_file.csv")
    )
  }

  return(dir)
}

read_proc_csv <- function(path,
                          header = TRUE,
                          accel_data = TRUE,
                          env = parent.frame()) {
  withr::defer(
    Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false"),
               env=env)
  Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")

  if (header) header <- readr::read_lines(path, n_max = 10)
  if (accel_data) accel_data <- suppressWarnings(readr::read_csv(path,
                                                                 skip = 10,
                                                show_col_types = FALSE))

  return(list(header = header, accel_data = accel_data))
}
