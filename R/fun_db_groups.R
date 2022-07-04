create_reporte_groups <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "groups")) {
    fields_list <- data.frame(
      group_id = strrep(" ", 64),
      group_description = strrep(" ", 256)
    )

    DBI::dbWriteTable(con, "groups", fields_list)
    group_remove(group_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'groups'")
  }

  DBI::dbDisconnect(con)
}

create_reporte_group_users <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "group_users")) {
    fields_list <- data.frame(
      group_id = strrep(" ", 64),
      user_id = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "group_users", fields_list)
    group_remove(group_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'group_users'")
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
    print(sprintf("inserted group with id '%s'", field_list$group_id))
  }
}

group_remove <- function(group_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM groups WHERE (group_id = '%s')", group_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted group with id %s", group_id))
}

gruser_insert <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "group_users", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
    print(sprintf("inserted user with id '%s' into '%s'", field_list$user_id, field_list$group_id))
  }
}

gruser_remove <- function(user_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM group_users WHERE (user_id = '%s')", user_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted group user with id %s", user_id))
}

gruser_get_groups <- function(user_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM group_users WHERE (user_id IN (%s))",
                                       db_collapse_vector(user_id)))
  DBI::dbDisconnect(con)
  return(data$group_id) # return a chr vector
}

gruser_get_from_group <- function(group_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM group_users WHERE (group_id IN (%s))",
                                       db_collapse_vector(group_id)))
  DBI::dbDisconnect(con)
  return(data$user_id) # return a chr vector
}

gruser_get_metadata <- function(group_id) {
  user_id_list <- gruser_get_from_group(group_id)
  data <- user_get_names(user_id_list)
  return(data)
}

# remove_table_from_reporte("groups")
# create_reporte_groups()

# create_reporte_group_users()

## Group insertion

# data.frame(
#   group_id = "team-ejemplo",
#   group_description = "Un ejemplo del funcionamiento de los grupos"
# ) |>
#   group_insert()

## Group user insertion

# data.frame(
#   group_id = "team-ejemplo",
#   user_id = "dgco93@mininter.gob.pe"
# ) |>
#   gruser_insert()

