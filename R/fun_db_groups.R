create_reporte_groups <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "groups")) {
    fields_list <- data.frame(
      group_id = strrep(" ", 64),
      group_description = strrep(" ", 256)
    )

    DBI::dbWriteTable(con, "groups", fields_list)
    group_remove(group_id = strrep(" ", 64))
    message("created table 'groups'")
  }

  DBI::dbDisconnect(con)
}



group_get_all <- function() {
    query <- "SELECT * FROM groups"
    data <- db_get_query(query)
    return(data)
}

group_insert <- function(field_list) {
    con <- db_connect()
    DBI::dbWriteTable(con, "groups", field_list, append = TRUE)
    DBI::dbDisconnect(con)
    glue::glue("inserted group with id {group_id}", group_id = field_list$group_id) |>
        message()
}

group_remove <- function(group_id) {
    statement <- "DELETE FROM groups WHERE (group_id = {group_id})"
    db_execute_statement(statement, group_id = group_id)
    glue::glue("deleted group with id {group_id}") |> message()
}

group_get_from_group_id <- function(group_id) {
    query <- "SELECT *
              FROM groups
              WHERE (group_id IN ({vals*}))"
    data <- db_get_query(query, vals = group_id)
    return(data)
}

group_get_description <- function(group_id) {
    query <- "SELECT group_description
              FROM groups
              WHERE (group_id IN ({vals*}))"
    data <- db_get_query(query, vals = group_id)
    return(data$group_description)
}

group_get_choices <- function(group_id){
    setNames(object = group_id,
             nm = group_id |> purrr::map_chr(group_get_description))
}


# db_remove_table("groups")
# create_reporte_groups()

# create_reporte_group_users()

## Group insertion

# data.frame(
#   group_id = "team-ejemplo",
#   group_description = "Un ejemplo del funcionamiento de los grupos"
# ) |>
#   group_insert()



