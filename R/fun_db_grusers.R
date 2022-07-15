create_reporte_group_users <- function() {
    con <- db_connect()

    if (!DBI::dbExistsTable(con, "group_users")) {
        fields_list <- data.frame(
            group_id = strrep(" ", 64),
            user_id = strrep(" ", 64)
        )

        DBI::dbWriteTable(con, "group_users", fields_list)
        group_remove(group_id = strrep(" ", 64))
        message("created table 'group_users'")
    }

    DBI::dbDisconnect(con)
}

gruser_insert <- function(field_list) {
    con <- db_connect()
    DBI::dbWriteTable(con, "group_users", field_list, append = TRUE)
    DBI::dbDisconnect(con)
    glue::glue("inserted user with id {user_id} into {group_id}",
               user_id = field_list$user_id,
               group_id = field_list$group_id) |> message()
}

gruser_remove <- function(group_id, user_id) {
    statement <- "DELETE FROM group_users
              WHERE (user_id = {user_id}) AND (group_id = {group_id})"
    db_execute_statement(statement, user_id = user_id, group_id = group_id)
    glue::glue("deleted group user with id {user_id} from {group_id}") |> message()
}

gruser_get_groups <- function(user_id) {
    query <- "SELECT *
            FROM group_users
            WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = user_id)
    return(data$group_id) # return a chr vector
}

gruser_get_from_group <- function(group_id) {
    query <- "SELECT *
            FROM group_users
            WHERE (group_id IN ({vals*}))"
    data <- db_get_query(query, vals = group_id)
    return(sort(data$user_id)) # return a chr vector
}

gruser_get_metadata <- function(group_id) {
    user_id_list <- gruser_get_from_group(group_id)
    data <- user_get_names(user_id_list)
    return(data)
}

gruser_purge <- function(user_id, with) {
    statement <- "DELETE FROM group_users
                  WHERE (user_id = {user_id})"
    db_execute_statement(statement, user_id = user_id)
    glue::glue("deleted group user with id {user_id}") |> message()
}

## Group user insertion

# data.frame(
#   group_id = "team-ejemplo",
#   user_id = "dgco93@mininter.gob.pe"
# ) |>
#   gruser_insert()

