# Turn off the logging
logger::log_threshold("FATAL")

test_that("save_header() matches a known good output", {
  dir <- local_dir_with_files(num_files = 1, inc_good = TRUE)

  # Read in the 'known good file'
  acti_file <- read_proc_csv(file.path(dir, "actilife_file.csv"),
    header = TRUE,
    accel_data = FALSE
  )

  # Process the same file in gt3x2csv
  actilife_ver <- "ActiLife v6.11.9"
  gt3x_file <- list_gt3x_rec(dir)
  read_gt3x_file <- read.gt3x::read.gt3x(gt3x_file)

  outfile <- file.path(dir, "test_header.csv")

  expect_error(
    save_header(read_gt3x_file,
      outfile = outfile,
      actilife = actilife_ver
    ),
    NA
  )
  test_header <- read_proc_csv(outfile, header = TRUE, accel_data = FALSE)
  expect_equal(test_header, acti_file)
})

test_that("save_accel() matches a known good output", {
  dir <- local_dir_with_files(num_files = 1, inc_good = TRUE)

  # Read in the 'known good file'
  acti_file <- read_proc_csv(file.path(dir, "actilife_file.csv"),
    header = FALSE,
    accel_data = TRUE
  )$accel_data

  # Process the same file in gt3x2csv
  gt3x_file <- list_gt3x_rec(dir)
  read_gt3x_file <- read.gt3x::read.gt3x(gt3x_file, imputeZeroes = TRUE)

  outfile <- file.path(dir, "test_accel.csv")

  expect_error(
    save_accel(read_gt3x_file,
      outfile = outfile
    ),
    NA
  )

  test_accel <- readr::read_csv(outfile, show_col_types = FALSE)

  # Too slow to compare the full data frame
  expect_equal(dim(test_accel), dim(acti_file))
  expect_equal(names(acti_file), names(test_accel))
  expect_identical(sum(test_accel[,1]), sum(acti_file[,1]))
  expect_identical(sum(test_accel[,2]), sum(acti_file[,2]))
  expect_identical(sum(test_accel[,3]), sum(acti_file[,3]))

})
