SessionData$set("public", "group_get_all", function() {
    query <- "SELECT * FROM groups"
    self$db_get_query(query)
})

SessionData$set("public", "group_insert", function(field_list) {
    DBI::dbWriteTable(self$con, "groups", field_list, append = TRUE)
    glue::glue("inserted group with id {field_list$group_id}") |>
        message()
})

SessionData$set("public", "group_remove", function(group_id) {
    statement <- "DELETE FROM groups WHERE (group_id = {group_id})"
    self$db_execute_statement(statement, group_id = group_id)
    glue::glue("deleted group with id {group_id}") |> message()
})

SessionData$set("public", "group_get_from_group_id", function(group_id) {
    query <- "SELECT *
              FROM groups
              WHERE (group_id IN ({vals*}))"
    self$db_get_query(query, vals = group_id)
})

SessionData$set("public", "group_get_description", function(group_id) {
    query <- "SELECT group_description
              FROM groups
              WHERE (group_id IN ({vals*}))"
    data <- self$db_get_query(query, vals = group_id)
    return(data$group_description)
})

SessionData$set("public", "group_get_choices", function(group_id){
    setNames(object = group_id,
             nm = group_id |> purrr::map_chr(self$group_get_description))
})
