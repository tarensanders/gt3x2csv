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
