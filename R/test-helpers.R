local_dir_with_files <- function(dir = tempdir(),
                                 num_files = 5,
                                 inc_good = FALSE,
                                 env = parent.frame()) {
  # Create a new directory
  path <- file.path(dir, "test_dir")

  dir.create(path)

  withr::defer(unlink(dir, recursive = TRUE), envir = env)

  # Add example files
  if (num_files > 0) {
    for (i in seq_len(num_files)) {
      file.copy(
        testthat::test_path("examples", "test_file.gt3x"),
        file.path(path, paste0("test_file", i, ".gt3x"))
      )
    }
  }

  # Add 'known good' file
  if (inc_good) {
    file.copy(
      testthat::test_path("examples", "actilife_file.csv"),
      file.path(path, "actilife_file.csv")
    )
  }

  return(path)
}

read_proc_csv <- function(path, header = TRUE, accel_data = TRUE) {
  if (header) header <- readr::read_lines(path, n_max = 10)
  if (accel_data) accel_data <- readr::read_csv(path, skip = 10,
                                                show_col_types = FALSE)

  return(list(header = header, accel_data = accel_data))
}
