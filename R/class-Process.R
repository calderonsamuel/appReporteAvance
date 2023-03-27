Process <- R6::R6Class(
    classname = "Process",
    inherit = Group,
    public = list(
        initialize = function(email) {
            super$initialize(email)
        },
        process_add = function(title, description = NA) {
            id <- ids::random_id()
            st <- glue::glue_sql(
                "INSERT INTO processes(group_id, process_id, title, description, created_by)
                VALUES ({self$group_selected}, {id}, {title}, {description}, {self$user$user_id})",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, st)
            
            return(id)
        },
        process_delete = function(process_id) {
            st <- glue::glue_sql(
                "DELETE FROM processes
                WHERE process_id = {process_id}",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, st)
        },
        process_edit = function(process_id, title, description) {
            st <- glue::glue_sql(
                "UPDATE processes
                SET 
                    title = {title},
                    description = {description}
                WHERE process_id = {process_id}",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, st)
        }
    )
)
