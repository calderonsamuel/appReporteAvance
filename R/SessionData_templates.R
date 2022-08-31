SessionData$set("public", "template_get_all", function() {
    query <- "SELECT * FROM templates"
    self$db_get_query(query)
})

SessionData$set("public", "template_insert", function(field_list) {
    DBI::dbWriteTable(self$con, "templates", field_list, append = TRUE)
    glue::glue("inserted template with id {template_id}", template_id = field_list$template_id) |>
        message()
})

SessionData$set("public", "template_remove", function(template_id) {
    statement <- "DELETE
                FROM templates
                WHERE (template_id = {template_id})"
    self$db_execute_statement(statement, template_id = template_id)
    glue::glue("deleted template with id {template_id}") |> message()
})

SessionData$set("public", "template_get_from_user", function(user_id) {
    query <-    "SELECT template_id
                FROM templates
                WHERE (user_id IN ({vals*}))"
    data <- self$db_get_query(query, vals = user_id)
    return(data$template_id)
})

SessionData$set("public", "template_get_description", function(template_id) {
    if (is.na(template_id)) return("Sin plantilla")
    query <- "SELECT template_description
              FROM templates
              WHERE (template_id IN ({vals*}))"
    data <- self$db_get_query(query, vals = template_id)
    return(data$template_description)
})

SessionData$set("public", "template_get_choices", function(template_id) {
    setNames(
        object = template_id,
        nm = template_id |> purrr::map_chr(self$template_get_description)
    )
})

SessionData$set("public", "template_get_data", function(template_id) {
    query <- "SELECT *
                FROM templates
                WHERE template_id IN ({vals*})"
    self$db_get_query(query, vals = template_id)
})
