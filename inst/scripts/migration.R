library(RMariaDB)
library(purrr)

con_to <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME_LATAM"),
    host = Sys.getenv("DB_HOST_LATAM"),
    port = Sys.getenv("DB_PORT")
)

# done with old config
con_from <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST_TEST"),
    port = Sys.getenv("DB_PORT")
)

tbl_names <- dbListTables(con_from) 

bd_data <- tbl_names |> 
    map(~dbReadTable(con_from, .x)) |> 
    set_names(tbl_names)

tbl_names |> 
    walk(~{
        dbWriteTable(con_to, .x, bd_data[[.x]], overwrite = TRUE)
        message(glue::glue("done with {.x}"))
    })

new_tbl_data <- tbl_names |> 
    map(~{
        dbReadTable(con_to, .x)
    }) |> 
    set_names(tbl_names)

dbDisconnect(con_to)
dbDisconnect(con_from)
