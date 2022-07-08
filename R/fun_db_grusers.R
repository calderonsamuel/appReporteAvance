create_reporte_group_users <- function() {
    con <- db_connect()

    if (!DBI::dbExistsTable(con, "group_users")) {
        fields_list <- data.frame(
            group_id = strrep(" ", 64),
            user_id = strrep(" ", 64)
        )

        DBI::dbWriteTable(con, "group_users", fields_list)
        group_remove(group_id = strrep(" ", 64), with_print = FALSE)
        message("created table 'group_users'")
    }

    DBI::dbDisconnect(con)
}

gruser_insert <- function(field_list, with_print = TRUE) {
    con <- db_connect()
    DBI::dbWriteTable(con, "group_users", field_list, append = TRUE)
    DBI::dbDisconnect(con)
    if (with_print) {
        message(sprintf("inserted user with id '%s' into '%s'", field_list$user_id, field_list$group_id))
    }
}

gruser_remove <- function(group_id, user_id, with_print = TRUE) {
    con <- db_connect()
    query <- glue::glue_sql("DELETE FROM group_users
                          WHERE (user_id = {user_id}) AND (group_id = {group_id})",
                          .con = con)
    DBI::dbExecute(con, query)
    DBI::dbDisconnect(con)
    if (with_print) message(glue::glue("deleted group user with id {user_id} from {group_id}"))
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
    return(sort(data$user_id)) # return a chr vector
}

gruser_get_metadata <- function(group_id) {
    user_id_list <- gruser_get_from_group(group_id)
    data <- user_get_names(user_id_list)
    return(data)
}

gruser_purge <- function(user_id, with) {
    con <- db_connect()
    query <- glue::glue_sql("DELETE FROM group_users
                          WHERE (user_id = {user_id})",
                          .con = con)
    DBI::dbExecute(con, query)
    DBI::dbDisconnect(con)
    message(glue::glue("deleted group user with id {user_id}"))
}

## Group user insertion

# data.frame(
#   group_id = "team-ejemplo",
#   user_id = "dgco93@mininter.gob.pe"
# ) |>
#   gruser_insert()

