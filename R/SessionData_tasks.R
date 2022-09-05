SessionData$set("public", "tasks_compute", function() {
    ids <- self$task_get_ids(self$user_id)
    metadata <- self$task_get_from_id(ids)
    
    # self$task_list_from_user()
    
    params <- list(
        task_id = metadata$task_id,
        task_description = metadata$task_description,
        task_status = metadata$status,
        task_creator_id = metadata$reviewer,
        task_creator_names = metadata$reviewer,
        task_assignee_id = metadata$user_id,
        task_assignee_names = metadata$user_id,
        template_id = metadata$template_id
    )
    
    purrr::pmap(params, list)
    # params
})

SessionData$set("public", "update_tasks", function() {
    self$tasks <- self$tasks_compute()
})

SessionData$set("public", "task_get_ids", function(user_id) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = user_id)
    return(data$task_id)
})

SessionData$set("public", "task_get_description", function() {
    query <- "SELECT task_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = self$user_id)
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

SessionData$set("public", "task_get_from_id", function(task_id) {
    query <- "SELECT task_id, task_description, status,
              user_id, reviewer, template_id
              FROM tasks
              WHERE (task_id IN ({vals*}))"
    private$db_get_query(query, vals = task_id)
})

SessionData$set("public", "task_modify_status", function(task_id, new_status) {
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

SessionData$set("public", "task_get_status", function(task_id) {
    query <- "SELECT status
              FROM tasks
              WHERE (task_id IN ({task_id}))"
    data <- private$db_get_query(query, task_id = task_id)
    return(data$status)
})

SessionData$set("public", "task_get_from_reviewer", function(reviewer) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (reviewer IN ({vals*}))"
    data <- private$db_get_query(query, vals = reviewer)
    return(data$task_id)
})

SessionData$set("public", "task_is_from_group", function(task_id) {
    query <- "SELECT user_id
            FROM tasks
            WHERE task_id = {task_id}"
    data <- private$db_get_query(query, task_id = task_id)
    return(grepl("^team", data$user_id))
})

SessionData$set("public", "task_get_from_status", function(user_id, status) {
    query <- "SELECT *
                FROM tasks
                WHERE (user_id IN ({vals*}) and status = {status})"
    private$db_get_query(query, vals = user_id, status = status)
})


