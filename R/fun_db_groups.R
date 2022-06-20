create_reporte_groups <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "groups")) {
    fields_list <- data.frame(
      group_id = strrep(" ", 64),
      group_description = strrep(" ", 256),
      user_id = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "groups", fields_list)
    delete_group(group_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'groups'")
  }

  DBI::dbDisconnect(con)
}

get_groups <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "groups")
  DBI::dbDisconnect(con)
  return(data)
}

insert_group <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "groups", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
    print(sprintf("inserted group with id %s", field_list$group_id))
  }
}

delete_group <- function(group_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM groups WHERE (group_id = '%s')", group_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted group with id %s", group_id))
}

get_groups_from_user <- function(user_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM groups WHERE (user_id IN (%s))",
                                       db_collapse_vector(user_id)))
  DBI::dbDisconnect(con)
  return(data)
}

# remove_table_from_reporte("groups")
# create_reporte_groups()
