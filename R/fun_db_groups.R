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
    message(sprintf("inserted group with id '%s'", field_list$group_id))
  }
}

group_remove <- function(group_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM groups WHERE (group_id = '%s')", group_id))
  DBI::dbDisconnect(con)
  if (with_print) message(sprintf("deleted group with id %s", group_id))
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



