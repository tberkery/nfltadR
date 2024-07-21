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
                       client.flag = RMySQL::CLIENT_LOCAL_FILES)
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

get_data_types = function(df) {
  sapply(df, function(column) {
    if (is.numeric(column)) {
      return("DOUBLE")
    } else if (is.integer(column)) {
      return("INT")
    } else if (is.character(column)) {
      return("VARCHAR(255)")
    } else if (lubridate::is.Date(column)) {
      return("DATE")
    } else {
      return("VARCHAR(255)")
    }
  })
}

write_data = function(df, db_name, table_name, con) {

  # Get data types
  data_types = get_data_types(df)

  # Generate SQL to create table if it does not exist
  create_table_sql = paste(
    glue::glue_sql("CREATE TABLE IF NOT EXISTS {`table_name`}", .con = con), "(",
    paste(paste(names(df), data_types), collapse = ", "),
    ", PRIMARY KEY (player_id, season)",
    ");"
  )

  # Create table if it does not exist
  DBI::dbExecute(con, create_table_sql)

  # Year to update
  update_years = unique(df$season)

  # Delete existing rows with the same year
  for (update_year in update_years) {
    delete_sql = glue::glue_sql("DELETE FROM {`table_name`} WHERE season = {update_year}", .con = con)
    DBI::dbExecute(con, delete_sql)
  }

  # Insert new data
  DBI::dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)
}





