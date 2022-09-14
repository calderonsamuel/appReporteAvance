library(RMariaDB)
library(purrr)

con <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT")
)

# done with old config
con2 <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST_OLD"),
    port = Sys.getenv("DB_PORT")
)

tbl_names <- dbListTables(con2) 

bd_data <- tbl_names |> 
    map(~dbReadTable(con2, .x)) |> 
    set_names(tbl_names)

tbl_names |> 
    walk(~dbWriteTable(con, .x, bd_data[[.x]]))

new_tbl_data <- tbl_names |> 
    map(~dbReadTable(con, .x)) |> 
    set_names(tbl_names)

dbDisconnect(con)
dbDisconnect(con2)
