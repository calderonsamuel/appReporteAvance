SessionData$set("public", "progress_get_all", function() {
    self$db_get_query("SELECT * FROM progress")
})

SessionData$set("public", "progress_insert", function(field_list, with_print = TRUE) {
    DBI::dbWriteTable(self$con, "progress", field_list, append = TRUE)
    if (with_print) {
        status_id <- field_list$status_id
        glue::glue("inserted status with ids {vals}", vals = status_id |> glue::glue_collapse(sep = ", ")) |>
            message()
    }
})

SessionData$set("public", "progress_remove", function(task_id, status_id, with_print = TRUE) {
    statement <-
        "DELETE
        FROM progress
        WHERE (status_id = {status_id} AND task_id = {task_id})"
    self$db_execute_statement(statement, status_id = status_id, task_id = task_id)
    
    glue::glue("deleted status with id '{status_id}' from '{task_id}'") |> message()
})

SessionData$set("public", "progress_get_step_status", function(task_id, step_id) {
    if (is.na(task_id)) return("Pendiente")
    query <-
        "SELECT status
        FROM progress
        WHERE (task_id = {task_id}
        AND step_id = {step_id})
        ORDER BY time DESC
        LIMIT 1"
    data <- self$db_get_query(query, task_id = task_id, step_id = step_id)
    return(data$status)
})

SessionData$set("public", "progress_status_choices", function(status) {
    status <- if (!isTruthy(status)) "Pendiente" else status
    if (status == "Pendiente") {
        c("En proceso", "Pausado")
    } else if (status == "En proceso") {
        c("Pausado", "En revisión")
    } else if (status == "Pausado") {
        c("En proceso", "En revisión")
    } else if (status == "En revisión") {
        c("En proceso", "Terminado")
    }
})


