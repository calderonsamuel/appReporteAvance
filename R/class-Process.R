Process <- R6::R6Class(
    classname = "Process",
    inherit = Group,
    public = list(
        initialize = function(email) {
            super$initialize(email)
        },
        fetch_processes = function() {
            st <- glue::glue_sql(
                "SELECT * 
                FROM processes
                WHERE group_id = {self$group_selected}",
                .con = private$con
            )

            DBI::dbGetQuery(private$con, st) |>
                purrr::pmap(list)
        },
        fetch_units = function(process_id) {
            st <- glue::glue_sql(
                "SELECT * 
                FROM units
                WHERE process_id = {process_id}",
                .con = private$con
            )

            DBI::dbGetQuery(private$con, st) |>
                purrr::pmap(list)
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
        },

        #' @description Add a measurement unit for a process
        unit_add = function(process_id, unit_title, unit_description = "",
                            unit_type, unit_icon = "file") {
            unit_id <- ids::random_id()
            unit_type <- match.arg(unit_type, c("report", "task"))
            
            st <- glue::glue_sql(
                "INSERT INTO units
                SET
                    process_id = {process_id},
                    unit_id = {unit_id},
                    unit_title = {unit_title},
                    unit_description = {unit_description},
                    type = {unit_type},
                    icon = {unit_icon},
                    creator = {self$user$user_id},
                    last_modified_by = {self$user$user_id}
                ",
                .con = private$con
            )

            DBI::dbExecute(private$con, st)
            
            if (interactive()) cli::cli_alert_info("Inserted unit '{unit_id}' into group '{self$group_selected}'")
            return(unit_id)
        },

        #' @description Edit a measurement unit from a process
        unit_edit = function(unit_id, unit_title, unit_description,
                            unit_type, unit_icon) {
            unit_type <- match.arg(unit_type, c("report", "task"))
            
            st <- glue::glue_sql("
                UPDATE units
                SET 
                    unit_title = {unit_title},
                    unit_description = {unit_description},
                    type = {unit_type},
                    icon = {unit_icon},
                    last_modified_by = {self$user$user_id}
                WHERE
                    unit_id = {unit_id}
            ",
            .con = private$con
            )

            DBI::dbExecute(private$con, st)
            
            if (interactive()) cli::cli_alert_info("Edited unit '{unit_id}' from '{self$group_selected}'")
        },

        #' @description Delete a measurement unit from a process
        unit_delete = function(unit_id) {
            st <- glue::glue_sql(
                "DELETE FROM units
                WHERE unit_id = {unit_id}",
                .con = private$con
            )

            DBI::dbExecute(private$con, st)
            
            if (interactive()) cli::cli_alert_info("Deleted unit '{unit_id}' from group '{self$group_selected}'")
        }
    )
)
