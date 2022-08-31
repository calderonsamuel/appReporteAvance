SessionData$set("public", "step_get_all", function() {
    self$db_get_query("SELECT * FROM templates_steps")
})

SessionData$set("public", "step_insert", function(field_list) {
    DBI::dbWriteTable(self$con, "templates_steps", field_list, append = TRUE)
    
    glue::glue("inserted steps {step_id} in template with id {template_id}",
               step_id = field_list$step_id,
               template_id = field_list$template_id) |>
        message(appendLF = TRUE)
})

SessionData$set("public", "step_remove", function(template_id) {
    statement <- "DELETE
                  FROM templates_steps
                  WHERE (template_id = {template_id})"
    self$db_execute_statement(statement, template_id = template_id)
    
    glue::glue("deleted template steps with id {template_id}") |> message()
})

SessionData$set("public", "step_get_from_template", function(template_id) {
    if(!shiny::isTruthy(template_id)) {
        data <- list(step_id = "step_01",
                     step_description = "Actividad única")
        return(data)
    }
    
    query <- "SELECT *
            FROM templates_steps
            WHERE (template_id = {template_id})"
    self$db_get_query(query, template_id = template_id)
})

SessionData$set("public", "step_get_description", function(template_id, step_id) {
    query <- "SELECT step_description
            FROM templates_steps
            WHERE (template_id = {template_id}
            AND step_id = {step_id})"
    data <- self$db_get_query(query, template_id = template_id, step_id = step_id)
    return(data$step_description)
})
