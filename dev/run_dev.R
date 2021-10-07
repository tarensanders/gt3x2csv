###############################################
# Default golem script for building R package #
###############################################
# Golem options
options(golem.app.prod = TRUE) # TRUE = production build, FALSE = dev build

# Cleanup R envir
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document + reload our package
golem::document_and_reload()

# Check consistency for CRAN
devtools::check()

# Build .tar.gz file for local install
devtools::build(path = here::here())