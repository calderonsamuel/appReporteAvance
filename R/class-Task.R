#' Get Group data
#'
#' R6 class that allows to get the Group information.
#'
#' @param email The email the user started the session with.
#' @param group_id The id of the group on which the statement will be executed
#' @param user_id The id of the user on which the statement will be executed
#' @param task_id The id of the task on which the statement will be executed
#' @param task_title The new title of the group
#' @param task_description The new description of the group
#' @param assignee The id of the user responsible for the task
#' @param time_due Deadline for the task completion. Datetime
#' @param output_unit Unit of measurement of the task output
#' @param output_goal Number on which which the output will be measured
#' @param status_current Current status of the specified task
#' @param output_current Current output of the specified task
#' @param output_progress Quantity of the progress being reported. Is measured in the unit specified in the creation of the task
#' @param status The status of the task once the new progress is added
#' @param details Explanation of the progress made
#' @importFrom cli cli_alert_success cli_h2
#' @importFrom purrr pmap keep map reduce
Task <- R6::R6Class(
    classname = "Task",
    inherit = Group,
    public = list(
        #' @description Start a Task based on an user email
        initialize = function(email) {
            super$initialize(email)
        },
        #' @description Add a new task for an User and report it as initial progress.
        task_add = function(group_id,
                            task_title, task_description, assignee, time_due,
                            output_unit, output_goal) {

            task_id <- ids::random_id()

            statement <-
                "INSERT INTO tasks
                SET
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
                    status_current = 'Pendiente'
                "
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) {
                cli::cli_h2("Task added")
                cli::cli_alert_info("group_id: {group_id}")
                cli::cli_alert_success("task_id: {task_id}")
            }
            
            return(task_id)
        },
        
        #' @description Delete an user task
        task_delete = function(task_id) {

            statement <-
                "DELETE FROM tasks
                WHERE
                    task_id = {task_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) {
                cli::cli_h2("Task deleted")
                cli::cli_alert_danger("task_id: {task_id}")
            }
        },

        #' @description Report progress on an assigned task
        task_report_progress = function(task_id,
                                        status_current, 
                                        output_current, 
                                        details) {

            statement <-
                "UPDATE tasks
                SET
                    status_current = {status_current},
                    output_current = {output_current}
                WHERE
                    task_id = {task_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) {
                cli::cli_h2("Task edited")
                cli::cli_alert_warning("task_id: {task_id}")
            }

            self$progress_add(task_id,
                              output_progress = output_current,
                              status = status_current,
                              details = details)
        },

        #' @description Edit a task metadata
        task_edit_metadata = function(task_id,
                                      task_title, 
                                      task_description) {

            statement <-
                "UPDATE tasks
                SET
                    task_title = {task_title},
                    task_description = {task_description}
                WHERE
                    task_id = {task_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) {
                cli::cli_h2("Task edited")
                cli::cli_alert_warning("task_id: {task_id}")
            }
        },
        
        #' @description Insert progress info on some task
        progress_add = function(task_id,
                                output_progress, 
                                status, 
                                details) {

            statement <-
                "INSERT INTO progress
                SET
                    task_id = {task_id},
                    reported_by = {self$user$user_id},
                    output_progress = {output_progress},
                    status = {status},
                    details = {details}"

            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) {
                cli::cli_h2("Progress reported")
                cli::cli_alert_warning("task_id: {task_id}")
                cli::cli_alert_warning("status: {status}")
            }
        },
        #' @description Get a task's progression history
        task_get_history = function(task_id) {
            subquery_progress <-
                "SELECT reported_by, output_progress, status, time_reported, details
                FROM progress
                WHERE task_id = {task_id}" |>
                super$db_make_query(task_id = task_id)

            query <-
                "SELECT output_progress, status, time_reported, details,
                        CONCAT(name, ' ', last_name) AS user_names
                FROM ({subquery}) AS lhs
                INNER JOIN users rhs ON
                    lhs.reported_by = rhs.user_id"

            super$db_get_query(query, subquery = subquery_progress)
        },
        #' @description Add a report and the quantities it has contributed in the specified units
        report_add = function(report_title, details, units, quantities) {
            
            if (length(units) != length(quantities)) {
                cli::cli_abort("`units` and `quantities` must be of the same size")
            }
            
            report_id <- ids::random_id()
            
            all_values <- glue::glue_sql("({report_id}, {units}, {quantities})", .con = private$con)
            
            report_insertion <- glue::glue_sql(
                "
                INSERT INTO reports(report_id, report_title, details, reported_by, group_id)
                VALUES
                    ({report_id}, {report_title}, {details}, {self$user$user_id}, {self$group_selected});
                ",
                .con = private$con
            )
            
            quantities_insertion <- glue::glue_sql(
                "
                INSERT INTO report_quantities(report_id, output_unit, output_progress)
                    VALUES
                        {all_values*};
                ",
                .con = private$con
            )
            
            DBI::dbBegin(private$con)
            
            DBI::dbExecute(conn = private$con, report_insertion)
            DBI::dbExecute(conn = private$con, quantities_insertion)
            
            DBI::dbCommit(private$con)
        },
        #' @description Delete a report and its contributions from the database.
        report_delete = function(report_id) {
            statement <- glue::glue_sql(
                "DELETE FROM reports
                WHERE report_id IN ({report_id*})",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, statement)
        },
        #' @description Archive a report and its contributions.
        report_archive = function(report_id) {
            statement <- glue::glue_sql(
                "UPDATE reports
                SET archived = 1
                WHERE 
                    report_id IN ({report_id*})",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, statement)
        }
    ),
    private = list(
        get_tasks = function() {
            query_tasks <- super$db_make_query(
                "SELECT *
                FROM tasks
                WHERE 
                    group_id = {group_id} AND
                    assignee IN ({assignees*}) AND
                        (status_current != 'Terminado' OR (
                            status_current = 'Terminado' AND
                            time_last_modified BETWEEN date_sub(now(), INTERVAL 2 WEEK) AND now()
                            )
                        )
                ",
                group_id = self$group_selected,
                assignees = private$get_assignees()
            )

            db_data <- super$db_get_query(
                "SELECT
                    lhs.*,
                    rhs.name AS assignee_name,
                    rhs.last_name AS assignee_last_name,
                    rhs2.name AS assigned_by_name,
                    rhs2.last_name AS assigned_by_last_name,
                    rhs3.user_color AS user_color
                FROM ({query_tasks}) AS lhs
                LEFT JOIN users rhs ON
                    lhs.assignee = rhs.user_id
                LEFT JOIN users rhs2 ON
                    lhs.assigned_by = rhs2.user_id
                LEFT JOIN group_users rhs3 ON
                    lhs.assignee = rhs3.user_id AND
                    lhs.group_id = rhs3.group_id
                ORDER BY lhs.time_last_modified DESC
                ",
                query_tasks = query_tasks
            )

            db_data |>
                purrr::pmap(list) |>
                setNames(nm = db_data$task_id)
        },
        get_assignees = function() {
            current_group <- self$group_users[[self$group_selected]]
            is_admin <- current_group[[self$user$user_id]][["group_role"]] == "admin"
            
            if (is_admin) {
                purrr::map_chr(current_group, "user_id")
            } else {
                self$user$user_id
            }
        }
    ),
    active = list(
        #' @field tasks List containing the tasks an User can interact with
        tasks = function() {
            private$get_tasks()
        },
        reports = function() {
            statement <- glue::glue_sql(
                "SELECT 
                    lhs.report_id, lhs.report_title, lhs.details, 
                    lhs.reported_by, lhs.time_reported, 
                    rhs.output_unit, rhs.output_progress
                FROM (
                    SELECT * FROM reports
                    WHERE 
                        group_id = {self$group_selected} AND
                        archived IS FALSE
                ) AS lhs
                LEFT JOIN report_quantities rhs ON
                    lhs.report_id = rhs.report_id
                ",
                .con = private$con
            )
            
            data <- DBI::dbGetQuery(private$con, statement)
            
            data |> 
                split(~report_id) |> 
                unname() |> 
                lapply(\(x) {
                    list(
                        report_id = x$report_id[1],
                        report_title = x$report_title[1],
                        details = x$details[1],
                        reported_by = x$reported_by[1],
                        time_reported = x$time_reported[1],
                        quantities = purrr::map2(x$output_unit, x$output_progress, ~list(
                            output_unit = .x,
                            output_progress = .y
                        ))
                    )
                })
        }
    )
)
