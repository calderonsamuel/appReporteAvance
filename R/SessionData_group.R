SessionData$set("public", "groups_compute", overwrite = TRUE, function() {
    ids <- self$group_get_ids()
    descriptions <- purrr::map_chr(ids, self$group_get_description)
    members_list <- purrr::map(ids, self$group_get_members_list)
    
    params <- list(
        group_id = ids,
        group_description = descriptions,
        group_members = members_list
    )
    
    purrr::pmap(params, \(group_id, group_description, group_members){
        list(
            group_id = group_id,
            group_description = group_description,
            group_members = group_members
        )
    })
})


SessionData$set("public", "group_get_ids", function() {
    query <- "SELECT *
            FROM group_users
            WHERE (user_id IN ({vals*}))"
    data <- private$db_get_query(query, vals = self$user_id)
    return(data$group_id) # return a chr vector
})

SessionData$set("public", "group_get_description", function(group_id) {
    query <- "SELECT group_description
              FROM groups
              WHERE (group_id IN ({vals*}))"
    data <- private$db_get_query(query, vals = group_id)
    return(data$group_description)
})

SessionData$set("public", "group_get_members_list", function(group_id) {
    query <- "SELECT *
            FROM group_users
            WHERE (group_id IN ({vals*}))"
    data <- db_get_query(query, vals = group_id)
    members_ids <- sort(data$user_id)
    members_names <- purrr::map_chr(members_ids, self$user_get_names)
    
    purrr::map2(members_ids, members_names, ~{
        list(
            user_id = .x,
            user_names = .y
        )
    })
})

SessionData$set("public", "group_get_all", function() {
    query <- "SELECT * FROM groups"
    private$db_get_query(query)
})

SessionData$set("public", "group_insert", function(field_list) {
    DBI::dbWriteTable(private$con, "groups", field_list, append = TRUE)
    glue::glue("inserted group with id {field_list$group_id}") |>
        message()
})

SessionData$set("public", "group_remove", function(group_id) {
    statement <- "DELETE FROM groups WHERE (group_id = {group_id})"
    private$db_execute_statement(statement, group_id = group_id)
    glue::glue("deleted group with id {group_id}") |> message()
})

SessionData$set("public", "group_get_from_group_id", function(group_id) {
    query <- "SELECT *
              FROM groups
              WHERE (group_id IN ({vals*}))"
    private$db_get_query(query, vals = group_id)
})

SessionData$set("public", "group_get_choices", function(group_id){
    setNames(object = group_id,
             nm = group_id |> purrr::map_chr(self$group_get_description))
})
