Task <- R6::R6Class(
    classname = "Task",
    inherit = Group,
    public = list(
        tasks = NULL,
        initialize = function(email) {
            super$initialize(email)
            self$tasks <- private$get_tasks()
        }
    ),
    private = list(
        get_tasks = function() {
            query_tasks <- super$db_make_query(
                "SELECT *
                FROM tasks 
                WHERE org_id IN ({orgs*}) AND
                    group_id IN ({groups*}) AND (
                        status_current != 'Terminado' OR (
                            status_current = 'Terminado' AND
                            time_last_modified BETWEEN date_sub(now(), INTERVAL 1 WEEK) AND now()
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
        }
    )
)
