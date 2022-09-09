SessionData$set("public", "groups_compute", function() {
    ids <- private$group_get_ids()
    group_df <- private$group_get_metadata(ids)
    members_list <- purrr::map(group_df$group_id, self$group_get_members_list)
    
    params <- list(
        group_id = group_df$group_id,
        group_description = group_df$group_description,
        group_admin = group_df$group_admin,
        group_members = members_list
    )
    
    purrr::pmap(params, list)
})


SessionData$set("private", "group_get_ids", function() {
    query <- "SELECT *
            FROM group_users
            WHERE (user_id IN ({vals*}))"
    data <- private$db_get_query(query, vals = self$user_id)
    ids <- data$group_id
    private$group_ids <- ids
    return(ids) # return a chr vector
})

SessionData$set("private", "group_get_metadata", function(group_ids) {
    query <- "SELECT *
              FROM groups
              WHERE (group_id IN ({vals*}))"
    private$db_get_query(query, vals = group_ids)
})

SessionData$set("private", "group_admined_members", function() {
    groups_owned <- self$groups |> 
        purrr::keep(~ .x$group_admin == self$user_id) 
    if (length(groups_owned) > 0) {
        groups_owned |> 
            purrr::map("group_members") |> 
            purrr::map(purrr::map_dfr, ~.x) |> 
            purrr::map_dfr(~.x) |> 
            (\(x) x$user_id)()
    } else {
        character()
    }
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

SessionData$set("private", "group_get_member_ids", function(group_ids) {
    query <- "SELECT *
            FROM group_users
            WHERE (group_id IN ({vals*}))"
    data <- db_get_query(query, vals = group_id)
    unique(data$user_id)
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
