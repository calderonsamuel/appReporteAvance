#' Get Group data
#'
#' R6 class that allows to get the Group information.
#'
#' @param email The email the user started the session with.
#' @param group_id The id of the group on which the statement will be executed
#' @param user_id The id of the user on which the statement will be executed
#' @param task_id The id of the task on which the statement will be executed
#' @param report_id The id of the report on which the statement will be executed
#' @param task_title Title for the task
#' @param task_description Long description of the task
#' @param assignee The id of the user responsible for the task
#' @param time_due Deadline for the task completion. Datetime
#' @param output_unit Unit of measurement of the task output
#' @param output_goal Number on which which the output will be measured
#' @param status_current Current status of the specified task
#' @param output_current Current output of the specified task
#' @param output_progress Quantity of the progress being reported. Is measured in the unit specified in the creation of the task
#' @param status The status of the task once the new progress is added
#' @param details Explanation of the progress made
#' @param report_title The title of the report
#' @param units Units of the report. Must have the same size as quantities.
#' @param quantities Quantities of the report. Must have the same size as units.
#' @param start_date Starting date of the report
#' @param end_date Ending date of the report
#' @importFrom cli cli_alert_success cli_h2
#' @importFrom purrr pmap keep map reduce
#' @importFrom rlang %||%
Task <- R6::R6Class(
    classname = "Task",
    inherit = Process,
    public = list(
        #' @description Start a Task based on an user email
        initialize = function(email) {
            super$initialize(email)
        },
        #' @description Add a new task for an User and report it as initial progress.
        task_add = function(group_id,
                            task_title, task_description, assignee, time_due,
                            output_unit) {

            task_id <- ids::random_id()

            task_st <- glue::glue_sql(
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
                    output_goal = {1L},
                    output_current = 0,
                    status_current = 'Pendiente'
                ",
                .con = self$con
            )

            details <- glue::glue(
                "Tarea creada con plazo máximo {plazo}",
                plazo = format(time_due, "%d/%m/%Y %H:%M:%S", tz = "America/Lima")
            )

            progress_st <- glue::glue_sql(
                "INSERT INTO progress
                SET
                    task_id = {task_id},
                    reported_by = {self$user$user_id},
                    output_progress = 0,
                    status = 'Pendiente',
                    details = {details}
                ",
                .con = self$con
            )

            DBI::dbBegin(self$con)
            DBI::dbExecute(self$con, task_st)
            DBI::dbExecute(self$con, progress_st)
            DBI::dbCommit(self$con)
            
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

        #' @description Edit a task metadata
        task_edit = function(task_id,
                                      task_title = NULL, 
                                      task_description = NULL,
                                      time_due = NULL) {
            
            # This keeps non null values and creates a SQL valid syntax
            edit_values <- list(
                task_title = task_title,
                task_description = task_description,
                time_due = time_due
            ) |>
                purrr::compact() |>
                purrr::imap_chr(\(x, i) {
                    glue::glue_sql("{`i`} = {x}", .con = self$con)
                }) |>
                glue::glue_sql_collapse(sep = ", ")

            st_task <- glue::glue_sql(
                "UPDATE tasks
                SET
                    {edit_values}
                WHERE
                    task_id = {task_id}",
                .con = self$con
            )

            # This keeps non null values and creates a valid details text
            details <- list(
                `Título` = task_title,
                `Descripción` = task_description,
                `Plazo máximo` = if (!is.null(time_due)) format(time_due, "%d/%m/%Y %H:%M:%S", tz = "America/Lima") else NULL
            ) |>
                purrr::compact() |>
                purrr::imap_chr(\(x, i) {
                    glue::glue("{i} cambiado a: '{x}'")
                }) |>
                stringr::str_trunc(200, ellipsis = "..'") |>
                stringr::str_flatten(collapse = "; ")

            # This reuses the last reported value for the progress when necessary
            st_progress <- glue::glue_sql(
                "INSERT INTO progress(task_id, reported_by, output_progress, status, details)
                SELECT 
                    task_id,
                    {self$user$user_id} AS reported_by,
                    output_progress,
                    status,
                    {details} AS details
                FROM progress
                WHERE task_id = {task_id}
                ORDER BY time_reported DESC
                LIMIT 1
                ",
                .con = self$con
            )

            DBI::dbBegin(self$con)
            DBI::dbExecute(self$con, st_task)
            DBI::dbExecute(self$con, st_progress)
            DBI::dbCommit(self$con)

            if (interactive()) {
                cli::cli_h2("Task edited")
                cli::cli_alert_warning("task_id: {task_id}")
                cli::cli_alert_info("time_due: {time_due}")
            }
        },

        #' @description Report progress on an assigned task
        task_report_progress = function(task_id,
                                        status_current,
                                        details) {

            statement <-
                "UPDATE tasks
                SET
                    status_current = {status_current}
                WHERE
                    task_id = {task_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) {
                cli::cli_h2("Task edited")
                cli::cli_alert_warning("task_id: {task_id}")
            }

            self$progress_add(task_id,
                              status = status_current,
                              details = details)
        },

        #' @description Archive a task
        task_archive = function(task_id) {
            task_st <- glue::glue_sql(
                "UPDATE tasks
                SET
                    status_current = 'Archivado'
                WHERE task_id = {task_id}
                ",
                .con = self$con
            )

            progress_st  <- glue::glue_sql(
                "INSERT INTO progress
                SET
                    task_id = {task_id},
                    reported_by = {self$user$user_id},
                    output_progress = 0,
                    status = 'Archivado',
                    details = 'Archivado manualmente'
                ",
                .con = self$con
            )

            DBI::dbBegin(self$con)
            DBI::dbExecute(self$con, task_st)
            DBI::dbExecute(self$con, progress_st)
            DBI::dbCommit(self$con)
        },
        
        #' @description Insert progress info on some task
        progress_add = function(task_id,
                                status, 
                                details) {

            output_progress <- if (status %in% c("Terminado", "Archivado")) 1L else 0L

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
            
            all_values <- glue::glue_sql("({report_id}, {units}, {quantities})", .con = self$con)
            
            report_insertion <- glue::glue_sql(
                "
                INSERT INTO reports(report_id, report_title, details, reported_by, group_id)
                VALUES
                    ({report_id}, {report_title}, {details}, {self$user$user_id}, {self$group_selected});
                ",
                .con = self$con
            )
            
            quantities_insertion <- glue::glue_sql(
                "
                INSERT INTO report_quantities(report_id, output_unit, output_progress)
                    VALUES
                        {all_values*};
                ",
                .con = self$con
            )
            
            DBI::dbBegin(self$con)
            
            DBI::dbExecute(conn = self$con, report_insertion)
            DBI::dbExecute(conn = self$con, quantities_insertion)
            
            DBI::dbCommit(self$con)
        },
        #' @description Delete a report and its contributions from the database.
        report_delete = function(report_id) {
            statement <- glue::glue_sql(
                "DELETE FROM reports
                WHERE report_id IN ({report_id*})",
                .con = self$con
            )
            
            DBI::dbExecute(self$con, statement)
        },
        #' @description Archive a report and its contributions.
        report_archive = function(report_id) {
            statement <- glue::glue_sql(
                "UPDATE reports
                SET archived = 1
                WHERE 
                    report_id IN ({report_id*})",
                .con = self$con
            )
            
            DBI::dbExecute(self$con, statement)
        },
        
        #' @description Get data for reporting
        fetch_reports_to_download = function(start_date, end_date) {
            
            st_reports <- glue::glue_sql(
                "
                SELECT *
                FROM reports 
                WHERE 
                    group_id = {self$group_selected} AND
                    time_reported BETWEEN {start_date} AND DATE_ADD({end_date}, INTERVAL 1 DAY)",
                .con = self$con
            )
            statement <- glue::glue_sql(
                "SELECT 
                    lhs.report_title,
                    rhs.output_unit, 
                    rhs.output_progress
                FROM ({st_reports}) AS lhs
                LEFT JOIN report_quantities rhs ON
                    lhs.report_id = rhs.report_id
                ",
                .con = self$con
            )
            
            DBI::dbGetQuery(self$con, statement)
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
                    status_current != 'Archivado'
                ",
                group_id = self$group_selected,
                assignees = private$get_assignees()
            )

            db_data <- super$db_get_query(
                "SELECT
                    lhs.*,
                    CONCAT(rhs.name, ' ', rhs.last_name) AS assignee_dn,
                    CONCAT(rhs2.name, ' ', rhs2.last_name) AS assigned_by_dn,
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
        #' @field reports List containing the reports an User can interact with
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
                .con = self$con
            )
            
            data <- DBI::dbGetQuery(self$con, statement)
            
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
