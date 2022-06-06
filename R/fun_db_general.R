ensure_reporte <- function() {
  if(!RSQLite::dbCanConnect(RSQLite::SQLite(), "reporte_avance.db")) print("created reporte_avance.db")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbDisconnect(con)
}

# ensure_reporte()


remove_table_from_reporte <- function(table) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbRemoveTable(con, table)
  RSQLite::dbDisconnect(con)
}
