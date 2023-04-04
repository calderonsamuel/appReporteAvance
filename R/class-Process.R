#' Process Class Definition
#' 
#' @description 
#' This is a class definition for the Process class. It inherits from the Group class.
#' 
Process <- R6::R6Class(
    classname = "Process",
    inherit = Group,
    public = list(

        #' @description
        #' This function initializes a Process object.
        #' It calls the initialize() method of the Group class to inherit its properties.
        #' 
        #' @param email character string with user's email address
        initialize = function(email) {
            super$initialize(email)
        },
        
        #' @description
        #' This function fetches all the processes that belong to a group using the group_selected property.
        #' It uses the glue and DBI libraries to execute an SQL query on the database connection stored in the private$con property.
        #' The results are transformed using purrr::pmap(list). 
        #' 
        #' @return a list of processed retrieved from the database
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
        
        #' @description
        #' This function fetches all the units that belong to a process identified by its ID using the glue and DBI libraries to execute an SQL query on the database connection stored in the private$con property.
        #' The results are transformed using purrr::pmap(list).
        #' 
        #' @param process_id integer with the ID of the process to retrieve units from
        #' @return a list of units retrieved from the database
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
        
        #' @description
        #' This function adds a process to the database using SQL queries executed through the glue and DBI libraries.
        #' It uses the ids library to generate random IDs for the new process and stores it in the ID column of the processes table.
        #' 
        #' @param title character string with the title of the process to add
        #' @param description character string with the description of the process (optional)
        #' @return the ID of the newly added process
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
        
        #' @description
        #' This function deletes a process from the database using an SQL query executed through the glue and DBI libraries.
        #' 
        #' @param process_id integer with the ID of the process to delete
        process_delete = function(process_id) {
            st <- glue::glue_sql(
                "DELETE FROM processes
                WHERE process_id = {process_id}",
                .con = private$con
            )
            
            DBI::dbExecute(private$con, st)
        },
        
        #' @description
        #' This function edits the title and description of a process identified by its ID in the database.
        #' It uses SQL queries to update the title and description columns of the processes table executed through the glue and DBI libraries.
        #' 
        #' @param process_id integer with the ID of the process to edit
        #' @param title character string with the new title for the process
        #' @param description character string with the new description for the process
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

        #' @description
        #' This function adds a measurement unit for a process identified by its ID to the database using SQL queries executed through the glue and DBI libraries.
        #' It uses the ids library to generate random IDs for the new unit and stores it in the ID column of the units table.
        #' It also checks if the unit type is either "report" or "task", and sets the "type" column of the units table accordingly.
        #' 
        #' @param process_id integer with the ID of the process to add a unit to
        #' @param unit_title character string with the title of the unit to add
        #' @param unit_description character string with the description of the unit (optional, defaults to "")
        #' @param unit_type character string with the type of the unit, must be either "report" or "task"
        #' @param unit_icon character string with the icon for the unit (optional, defaults to "file")
        #' @return the ID of the newly added unit
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

        #' @description
        #' This function edits the title, description, icon and/or type of a measurement unit identified by its ID in the database.
        #' It uses SQL queries to update the title, description, icon and/or type columns of the units table executed through the glue and DBI libraries.
        #' It also checks if the unit type is either "report" or "task", and sets the "type" column of the units table accordingly.
        #' 
        #' @param unit_id integer with the ID of the unit to be edited
        #' @param unit_title character string with the new title for the unit
        #' @param unit_description character string with the new description for the unit
        #' @param unit_type character string with the new type of the unit, must be either "report" or "task"
        #' @param unit_icon character string with the new icon for the unit
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

        #' @description
        #' This function deletes a measurement unit identified by its ID from the database using an SQL query executed through the glue and DBI libraries.
        #' 
        #' @param unit_id integer with the ID of the unit to delete
        #' @export
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
