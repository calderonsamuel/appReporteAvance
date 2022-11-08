Task <- R6::R6Class(
    classname = "Task",
    inherit = Group,
    public = list(
        # tasks = NULL,
        initialize = function(email) {
            super$initialize(email)
            # private$sync_tasks()
        },
        task_add = function(process_id = NA_character_, activity_id = NA_character_, 
                            org_id, group_id, 
                            task_title, task_description, assignee, time_due,
                            output_unit, output_goal) {
            
            if (xor(is.na(process_id), is.na(activity_id))) {
                rlang::abort("`process_id` y `activity_id` deben ser o NA o chr en simultáneo")
            }
            
            task_id <- ids::random_id() 
            t_stamp <- super$get_timestamp()
            
            statement <- 
                "INSERT INTO tasks
                SET
                    process_id = {process_id},
                    activity_id = {activity_id},
                    org_id = {org_id},
                    group_id = {group_id},
                    task_id = {task_id},
                    task_title = {task_title},
                    task_description = {task_description},
                    assigned_by = {self$user$user_id},
                    assignee = {assignee},
                    time_due = {time_due},
                    output_unit = {output_unit},
                    output_goal = {output_goal},
                    output_current = 0,
                    status_current = 'Pendiente',
                    time_creation = {t_stamp},
                    time_last_modified = {t_stamp}
                "
            super$db_execute_statement(statement, .envir = rlang::current_env())

            cli::cli_h2("Task added")
            cli::cli_alert_info("process_id: {process_id}")
            cli::cli_alert_info("activity_id: {activity_id}")
            cli::cli_alert_info("org_id: {org_id}")
            cli::cli_alert_info("group_id: {group_id}")
            cli::cli_alert_success("task_id: {task_id}")
        },
        task_delete = function(process_id = NA_character_, activity_id = NA_character_,
                               org_id, group_id, task_id) {
            
            private$check_process(process_id, activity_id)
            
            statement <- 
                "DELETE FROM tasks
                WHERE
                    org_id = {org_id} AND
                    group_id = {group_id} AND
                    task_id = {task_id}"
            
            if (!is.na(process_id) && !is.na(activity_id)) {
                statement <- 
                    paste0(statement, 
                        " AND process_id = {process_id} AND activity_id = {activity_id}")
            }
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            cli::cli_h2("Task deleted")
            cli::cli_alert_info("process_id: {process_id}")
            cli::cli_alert_info("activity_id: {activity_id}")
            cli::cli_alert_info("org_id: {org_id}")
            cli::cli_alert_info("group_id: {group_id}")
            cli::cli_alert_danger("task_id: {task_id}")
        },
        task_report_progress = function(process_id = NA_character_, activity_id = NA_character_,
                               org_id, group_id, task_id, status_current, output_current) {
            
            private$check_process(process_id, activity_id)
            
            t_stamp <- super$get_timestamp()
            
            statement <- 
                "UPDATE tasks
                SET
                    status_current = {status_current},
                    output_current = {output_current},
                    time_last_modified = {t_stamp}
                WHERE
                    org_id = {org_id} AND
                    group_id = {group_id} AND
                    task_id = {task_id}"
            
            if (!is.na(process_id) && !is.na(activity_id)) {
                statement <- 
                    paste0(statement, 
                           " AND process_id = {process_id} AND activity_id = {activity_id}")
            }
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            cli::cli_h2("Task edited")
            cli::cli_alert_info("process_id: {process_id}")
            cli::cli_alert_info("activity_id: {activity_id}")
            cli::cli_alert_info("org_id: {org_id}")
            cli::cli_alert_info("group_id: {group_id}")
            cli::cli_alert_warning("task_id: {task_id}")
        }
    ),
    private = list(
        get_tasks = function() {
            query_tasks <- super$db_make_query(
                "SELECT *
                FROM tasks 
                WHERE org_id IN ({orgs*}) AND
                    group_id IN ({groups*}) AND 
                    assignee = {self$user$user_id} AND
                        (status_current != 'Terminado' OR (
                            status_current = 'Terminado' AND
                            time_last_modified BETWEEN date_sub(now(), INTERVAL 2 WEEK) AND now()
                            )
                        )",
                orgs = names(self$orgs),
                groups = names(self$groups)
            )
            
            db_data <- super$db_get_query(
                "SELECT 
                    lhs.*,
                    rhs.name AS assignee_name,
                    rhs.last_name AS assignee_last_name,
                    rhs2.name AS assigned_by_name,
                    rhs2.last_name AS assigned_by_last_name
                FROM ({query_tasks}) AS lhs
                LEFT JOIN users rhs ON
                    lhs.assignee = rhs.user_id
                LEFT JOIN users rhs2 ON
                    lhs.assigned_by = rhs2.user_id",
                query_tasks = query_tasks
            ) 
            
            db_data |> 
                purrr::pmap(list) |> 
                setNames(nm = db_data$task_id)
        },
        check_process = function(process_id, activity_id) {
            if (xor(is.na(process_id), is.na(activity_id))) {
                rlang::abort("`process_id` y `activity_id` deben ser o NA o chr en simultáneo")
            }
        }
    ),
    active = list(
        tasks = function() {
            private$get_tasks()
        }
    )
)
