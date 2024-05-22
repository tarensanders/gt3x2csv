convert_agd_file <- function(agd_file, outfile, cores) {
  con <- DBI::dbConnect(RSQLite::SQLite(), agd_file)

  # Call save_agd_header
  # Call save_agd_data
  # Close the connection
}

save_agd_header <- function(con, outfile) {
  settings_table <- DBI::dbReadTable(con, "settings")

  # Pull the required data out from the table
  # Format the data using glue into a header
  # Write the header to the outfile
}

save_agd_data <- function(con, outfile, cores) {
  data_table <- DBI::dbReadTable(con, "data")

  # Select only the data that is needed and pull it out
  # Write the data to the outfile
}
