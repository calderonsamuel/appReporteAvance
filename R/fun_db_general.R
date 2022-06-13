db_connect <- function() {
  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"))
}

db_list_tables <- function() {
  con <- db_connect()
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  return(tables)
}

# ensure_reporte <- function() {
#   if(!RSQLite::dbCanConnect(RSQLite::SQLite(), "reporte_avance.db")) print("created reporte_avance.db")
#   con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
#   RSQLite::dbDisconnect(con)
# }

# ensure_reporte()


remove_table_from_reporte <- function(table) {
  con <- db_connect()
  DBI::dbRemoveTable(con, table)
  DBI::dbDisconnect(con)
  print(sprintf("deleted table '%s'", table))
}


