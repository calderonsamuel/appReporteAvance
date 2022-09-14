task_list_from_user <- function(user_id = "dgco93@mininter.gob.pe"){
# user_id debe ser character de length = 1
    task_list_for_board(user_id) |>
        task_metadata()
}

task_list_subset_by_status <- function(task_list, status) {
    index <- task_list |> lapply(\(x) x$status == status) |> unlist()
    task_list[index]
}

progress_step_status_list <- function(task_id) {
    template_id <- task_get_from_id(task_id)$template_id
    step_list_ids <- step_get_from_template(template_id)$step_id
    data <- step_list_ids |>
        lapply(\(x) list(
            step_id = x,
            step_status = progress_get_step_status(task_id, x)
        )) |>
        setNames(step_list_ids)
    return(data)
}

task_compute_status <- function(task_id) {
    step_status <- progress_step_status_list(task_id) |>
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
}

task_metadata <- function(task_id) {
    data <- task_get_from_id(task_id) |>
        setNames(c("task_id", "task_description", "status", "assignee", "reviewer", "template")) |>
        split(~task_id) |>
        lapply(as.list)

    data <- data |>
        lapply(\(x) {
            x$assignee <- list(
                user_id = x$assignee,
                user_name = user_get_names(x$assignee)
            )
            x$reviewer <- list(
                user_id = x$reviewer,
                user_name = user_get_names(x$reviewer)
            )
            x$template <- list(
                template_id = x$template,
                template_description = template_get_description(x$template)
            )
            # x$steps <- progress_step_status_list(x$task_id, x$template$template_id)
            return(x)
        })


    return(data)
}
