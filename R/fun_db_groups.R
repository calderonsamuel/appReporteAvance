create_reporte_groups <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "groups")) {
    fields_list <- data.frame(
      group_id = strrep(" ", 64),
      group_description = strrep(" ", 256)
    )

    DBI::dbWriteTable(con, "groups", fields_list)
    group_remove(group_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'groups'")
  }

  DBI::dbDisconnect(con)
}



group_get_all <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "groups")
  DBI::dbDisconnect(con)
  return(data)
}


group_insert <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "groups", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
      group_id <- field_list$group_id
      glue::glue("inserted group with id {group_id}") |> message()
  }
}

group_remove <- function(group_id, with_print = TRUE) {
  con <- db_connect()
  statement <- glue::glue_sql("DELETE FROM groups WHERE (group_id = {group_id})")
  DBI::dbExecute(con, statement)
  DBI::dbDisconnect(con)
  if (with_print) glue::glue("deleted group with id {group_id}") |> message()
}

group_get_from_group_id <- function(group_id) {
  con <- db_connect()
  query <- glue::glue_sql("SELECT *
                          FROM groups
                          WHERE (group_id IN ({vals*}))",
                          vals = group_id,
                          .con = con)
  data <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  return(data)
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



