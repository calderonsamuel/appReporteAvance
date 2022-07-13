task_list_from_user <- function(user_id = "dgco93@mininter.gob.pe"){
    data <- task_get_from_user(user_id) |>
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
            return(x)
        })


    return(data)
}

task_list_subset_by_status <- function(task_list, status) {
    index <- task_list |> lapply(\(x) x$status == status) |> unlist()
    task_list[index]
}
