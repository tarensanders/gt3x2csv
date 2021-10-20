# Turn off the logging
logger::log_threshold("FATAL")

test_that("check_file_input() correctly identifies inputs", {
  dir <- local_dir_with_files()
  test_vec <- list.files(dir, full.names = TRUE)

  expect_equal(check_file_input(dir), "directory")
  expect_equal(check_file_input(test_vec), "vector")
  expect_equal(check_file_input(test_vec[1]), "single")
})

test_that("validate_gt3x_files() detects errors", {
  dir <- local_dir_with_files()
  test_vec <- list.files(dir, full.names = TRUE)

  expect_error(validate_gt3x_files(test_vec, "vector"), NA)
  expect_error(validate_gt3x_files(test_vec[1], "single"), NA)

  # Add a file that is missing info
  dummy_file <- file.path(dir, "dummy.csv")
  file.create(dummy_file)
  utils::zip(file.path(dir, "bad.gt3x"), dummy_file, flags = "-q")

  test_vec <- list.files(dir, full.names = TRUE)

  expect_error(suppressMessages(validate_gt3x_files(test_vec, "vector")))
})


test_that("list_gt3x_rec() finds all files", {
  dir <- local_dir_with_files()
  # Add a dummy file
  file.create(file.path(dir, "dummy_file"))

  expect_equal(length(list_gt3x_rec(dir, recursive = FALSE)), 5)

  # Add a nested directory
  nested_dir <- file.path(dir, "nested")
  dir.create(nested_dir)
  file.copy(
    testthat::test_path("examples", "test_file.gt3x"),
    file.path(nested_dir, "test_file.gt3x")
  )

  expect_equal(length(list_gt3x_rec(dir, recursive = TRUE)), 6)
})

test_that("generate_outputfiles() creates correct outputs if no outdir", {
  dir <- local_dir_with_files()
  infiles <- list_gt3x_rec(dir)
  outfiles <- generate_outputfiles(infiles, outdir = NULL)

  expect_vector(outfiles, size = length(infiles))
  expect_equal(dirname(outfiles), dirname(infiles))
  expect_equal(length(grep("RAW.csv$", outfiles)), length(infiles))
})

test_that("generate_outputfiles() creates outputs in correct dir", {
  dir <- local_dir_with_files()
  nested_dir <- file.path(dir, "nested")
  dir.create(nested_dir)

  infiles <- list_gt3x_rec(dir)
  outfiles <- generate_outputfiles(infiles, outdir = nested_dir)

  expect_vector(outfiles, size = length(infiles))
  expect_equal(
    normalizePath(dirname(outfiles[1]), mustWork = FALSE),
    normalizePath(nested_dir, mustWork = FALSE)
  )
  expect_equal(length(grep("RAW.csv$", outfiles)), length(infiles))
})
