# Establish a connection
connect_db = function(db_name = "fantasy_football") {
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

disconnect_db = function(con) {
  DBI::dbDisconnect(con)
}





