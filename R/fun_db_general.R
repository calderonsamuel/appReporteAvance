db_connect <- function(remote = getOption("reporte_avance.remote")) {
    if (remote) {
        DBI::dbConnect(
            drv = RMariaDB::MariaDB(),
            user = Sys.getenv("DB_USER"),
            password = Sys.getenv("DB_SECRET"),
            dbname = Sys.getenv("DB_NAME"),
            host = Sys.getenv("DB_HOST"),
            port = Sys.getenv("DB_PORT")
        )
    } else {
        DBI::dbConnect(drv = RSQLite::SQLite(), dbname = Sys.getenv("DB_NAME"))
    }
}

db_list_tables <- function() {
  con <- db_connect()
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  return(tables)
}

db_remove_table <- function(table) {
  con <- db_connect()
  DBI::dbRemoveTable(con, table)
  DBI::dbDisconnect(con)
  glue::glue("deleted table '{table}'") |> message()
}

db_get_query <- function(query_str, vals = NULL) {
    con <- db_connect()
    if (is.null(vals)) {
        query <- glue::glue_sql(query_str, .con = con, vals = vals)
    } else {
        query <- glue::glue_sql(query_str, .con = con)
    }

    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
}

db_execute_statement <- function(statement_str, vals = NULL) {
    con <- db_connect()
    if (is.null(vals)) {
        statement <- glue::glue_sql(statement_str, .con = con, vals = vals)
    } else {
        statement <- glue::glue_sql(statement_str, .con = con)
    }

    DBI::dbExecute(con, statement)
    DBI::dbDisconnect(con)
}
