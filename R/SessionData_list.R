SessionData$set("public", "task_list_subset_by_status", function(task_list, status) {
    index <- task_list |> lapply(\(x) x$status == status) |> unlist()
    task_list[index]
})

SessionData$set("public", "progress_step_status_list", function(task_id) {
    template_id <- private$task_get_from_id(task_id)$template_id
    step_list_ids <- self$step_get_from_template(template_id)$step_id
    data <- step_list_ids |>
        lapply(\(x) list(
            step_id = x,
            step_status = self$progress_get_step_status(task_id, x)
        )) |>
        setNames(step_list_ids)
    return(data)
})

SessionData$set("public", "task_compute_status", function(task_id) {
    step_status <- self$progress_step_status_list(task_id) |>
        purrr::map_chr(~.x$step_status)
    
    if (all(step_status == "Pendiente")) {
        "Pendiente"
    } else if (all(step_status == "Terminado")) {
        "Terminado"
    } else if (all(step_status == "En revisión")) {
        "En revisión"
    } else if (any(step_status == "Pausado")){
        "Pausado"
    } else {
        "En proceso"
    }
})

SessionData$set("public", "task_get_choices", function() {
    user_id <- self$user_id
    privileges <- self$privileges
    groups <- self$groups |> purrr::map_chr(~.x$group_id)
    
    users_for_tasks <- if (privileges == "user1") user_id else self$user_get_from_privileges(c("user1", "user2"))
    
    template_owners <- union(user_id, groups)
    
    user_choices <- self$user_get_choices(users_for_tasks)
    group_choices <- self$group_get_choices(groups)
    
    template_choices <- template_owners |>
        self$template_get_from_user() |>
        self$template_get_choices()
    
    list(
        user_choices = user_choices,
        group_choices = group_choices,
        template_choices = template_choices
    )
})

SessionData$set("public", "user_get_task_owners", function(user_id) {
    privileges <- self$privileges
    groups <- self$groups
    
    users_for_tasks <- if (privileges == "user1") user_id else self$user_get_from_privileges(c("user1", "user2"))
    
    task_owners <- union(users_for_tasks, groups)
    
    # glue::glue("task_owners: {vals}", vals = glue::glue_collapse(task_owners, sep = ", ")) |> message()
    
    return(task_owners)
})

# task_list_from_user()
