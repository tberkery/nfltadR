args = commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments is provided
if (length(args) != 2) {
  stop("Two arguments must be provided: min_year and max_year", call. = FALSE)
}

# Assign command-line arguments to variables
min_year <- as.integer(args[1])
max_year <- as.integer(args[2])

futile.logger::flog.info(glue::glue("Min year: {min_year}"))
futile.logger::flog.info(glue::glue("Max year: {max_year}"))

devtools::load_all()
get_data(min_year, max_year)
