# Establish a connection
connect_read_db = function(db_name = "fantasy_football") {
  con = DBI::dbConnect(RMySQL::MySQL(),
                       dbname = db_name,
                       host = "localhost",
                       user = "tberkery",
                       password = Sys.getenv("MYSQL_FANTASY_FOOTBALL_PW"))
  # Query to check if the database exists
  db_exists = DBI::dbGetQuery(con, "
      SELECT SCHEMA_NAME
      FROM INFORMATION_SCHEMA.SCHEMATA
      WHERE SCHEMA_NAME = 'fantasy_football';
  ")
  if (nrow(db_exists) > 0) {
    futile.logger::flog.info("Database exists")
  } else {
    futile.logger::flog.info("Database does not exist")
  }
  return(con)
}

connect_write_db = function(db_name = "fantasy_football") {
  con = DBI::dbConnect(RMySQL::MySQL(),
                       dbname = db_name,
                       host = "localhost",
                       user = "tberkery",
                       password = Sys.getenv("MYSQL_FANTASY_FOOTBALL_PW"),
                       default.file = "~/.my.cnf")
  # Query to check if the database exists
  db_exists = DBI::dbGetQuery(con, "
    SELECT SCHEMA_NAME
    FROM INFORMATION_SCHEMA.SCHEMATA
    WHERE SCHEMA_NAME = 'fantasy_football';
")
  if (nrow(db_exists) > 0) {
    futile.logger::flog.info("Database exists")
  } else {
    futile.logger::flog.info("Database does not exist")
  }
  return(con)
}

disconnect_read_db = function(con) {
  DBI::dbDisconnect(con)
}

write_data = function(data, db_name, table_name, con) {
  db_exists = DBI::dbGetQuery(con, glue::glue_sql("
      SELECT SCHEMA_NAME
      FROM INFORMATION_SCHEMA.SCHEMATA
      WHERE SCHEMA_NAME = {db_name};
  ", .con = con))
  if (nrow(db_exists) > 0) {
    futile.logger::flog.info("Database exists")

    # Check if the table exists
    table_exists = DBI::dbExistsTable(con, table_name)

    if (!table_exists) {
      futile.logger::flog.info(paste("Table", table_name, "does not exist. Creating table."))

      # Create table based on the data frame structure
      DBI::dbWriteTable(con, table_name, data, overwrite = TRUE, row.names = FALSE)
    } else {
      futile.logger::flog.info(paste("Table", table_name, "exists. Checking for overlapping years."))

      # Assuming there's a 'season' column in the data
      years_in_data = unique(data$season)

      # Retrieve existing years in the database table
      existing_years_query = sprintf("SELECT DISTINCT season FROM %s", table_name)
      existing_years = DBI::dbGetQuery(con, existing_years_query)$season

      # Find overlapping years
      overlapping_years = intersect(years_in_data, existing_years)

      if (length(overlapping_years) > 0) {
        futile.logger::flog.info(paste("Overlapping years found:", paste(overlapping_years, collapse = ", "), ". Deleting existing records for these years."))

        # Delete existing records for overlapping years
        delete_query = sprintf("DELETE FROM %s WHERE season IN (%s)", table_name, paste(overlapping_years, collapse = ", "))
        DBI::dbExecute(con, delete_query)
      }

      # Append the new data
      futile.logger::flog.info(paste("Appending data to table", table_name, "."))
      DBI::dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
    }

    futile.logger::flog.info(paste("Data written to table", table_name, "successfully."))
  } else {
    futile.logger::flog.info("Database does not exist")
  }
}





