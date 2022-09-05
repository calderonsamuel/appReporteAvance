SessionData$set("private", "tasks_compute", function() {
    ids <- private$task_get_ids(self$user_id)
    metadata <- private$task_get_from_id(ids)
    
    params <- list(
        task_id = metadata$task_id,
        task_description = metadata$task_description,
        task_status = metadata$status,
        task_creator_id = metadata$reviewer,
        task_creator_names = metadata$reviewer |> purrr::map_chr(self$user_get_names),
        task_assignee_id = metadata$user_id,
        task_assignee_names = metadata$user_id |> purrr::map_chr(self$user_get_names),
        template_id = metadata$template_id,
        template_description = purrr::map_chr(metadata$template_id, self$template_get_description),
        is_from_group = grepl("^team", metadata$user_id)
    )
    
    purrr::pmap(params, list) |> 
        setNames(metadata$task_id)
})

SessionData$set("public", "update_tasks", function() {
    self$tasks <- private$tasks_compute()
    private$task_update_tracker <- private$task_update_tracker + 1L
    invisible(self)
})

SessionData$set("public", "task_update_getter", function() {
    private$task_update_tracker
})

SessionData$set("public", "task_by_status", function(status) {
    self$tasks |> 
        purrr::keep(~ .x$task_status == status)
})

SessionData$set("private", "task_get_ids", function(user_id) {
    group_ids <- self$groups |> purrr::map_chr(~.x$group_id)
    task_owner_ids <- c(user_id, group_ids)
    query <- "SELECT task_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = task_owner_ids)
    return(data$task_id)
})

SessionData$set("public", "task_get_all", function() {
    private$db_get_query("SELECT * FROM tasks")
})

SessionData$set("public", "task_insert", function(task_list) {
    DBI::dbWriteTable(private$con, "tasks", task_list, append = TRUE)
    glue::glue("inserted task with id {task_id}", task_id = task_list$task_id) |>
        message()
})

SessionData$set("public", "task_remove", function(task_id) {
    statement <- "DELETE
                  FROM tasks
                  WHERE (task_id = {task_id})"
    private$db_execute_statement(statement, task_id = task_id)
    glue::glue("deleted task with id {task_id}") |> message()
})

SessionData$set("private", "task_get_from_id", function(task_id) {
    query <- "SELECT task_id, task_description, status,
              user_id, reviewer, template_id
              FROM tasks
              WHERE (task_id IN ({vals*}))"
    private$db_get_query(query, vals = task_id)
})

SessionData$set("public", "task_modify_status", function(task_id) {
    new_status <- self$task_compute_status(task_id)
    statement <- "UPDATE tasks
                SET status = {new_status}
                WHERE task_id = {task_id}"
    private$db_execute_statement(statement, new_status = new_status, task_id = task_id)
    glue::glue("modified task with id {task_id}") |> message()
})

SessionData$set("public", "task_get_from_user", function(user_id) {
    query <- "SELECT task_id, task_description, status,
              user_id, reviewer, template_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    private$db_get_query(query, vals = user_id)
})

SessionData$set("public", "task_get_from_user2", function(user_id) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- private$db_get_query(query, vals = user_id)
    return(data$task_id)
})

SessionData$set("public", "task_get_from_reviewer", function(reviewer) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (reviewer IN ({vals*}))"
    data <- private$db_get_query(query, vals = reviewer)
    return(data$task_id)
})


