#' Get Group data
#'
#' R6 class that allows to get the Group information.
#'
#' @param email The email the user started the session with.
#' @param org_id The id of the organisation on which the statement will be executed
#' @param group_id The id of the group on which the statement will be executed
#' @param user_id The id of the user on which the statement will be executed
#' @param user_color The color of the user's cards
#' @param group_title The new title of the group
#' @param group_description The new description of the group
#' @param group_role The role for the user in the group
#' @param unit_id The id of a group's measurement unit
#' @param unit_title The title of a group's measurement unit
#' @param unit_description The description of a group's measurement unit
#' @param unit_type The type of a group's measurement unit. One of ("report", "task")
#' @param unit_icon The icon of a group's measurement unit. Should be the compatible with fontawesome::fa().
Group <- R6::R6Class(
    classname = "Group",
    inherit = Organisation,
    public = list(
        #' @field group_selected ID of the group to be used in the board, by default is the favorite group in group_users
        group_selected = NULL,

        #' @description Start a Group based on an user email
        initialize = function(email = Sys.getenv("REPORTES_EMAIL")) {
            super$initialize(email)
            
            favorite_group <- super$db_get_query(
                "SELECT group_id 
                FROM group_users 
                WHERE 
                    user_id = {self$user$user_id} AND
                    favorite_group IS TRUE")
            
            if (length(favorite_group$group_id) == 1) {
                self$group_selected <- favorite_group$group_id
            } else {
                self$group_selected <- self$groups[[1]]$group_id
            }
            
            
        },
        #' @description Initialize a group for a new user
        group_initialize = function(org_id) {
            group_id <- self$group_add(org_id, )
            
            self$group_select(group_id)

            self$group_user_add(
                org_id = org_id,
                group_id = group_id,
                user_id = self$user$user_id,
                group_role = "admin"
            ) |> suppressMessages()
            
            private$group_add_default_units()

            if (interactive()) cli::cli_alert_info("Initialized group '{group_id}' in org '{org_id}' by user '{self$user$user_id}'")
        },
        
        #' @description Add a group to the database
        group_add = function(org_id, group_title, group_description) {
            group_id <- ids::random_id()
            
            statement <-
                "INSERT INTO groups
                SET
                    org_id = {org_id},
                    group_id = {group_id},
                    group_title = {group_title},
                    group_description = {group_description},
                    parent_group = 'organisation'"
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            return(group_id)
        },
        
        #' @description Remove a group from the database
        group_delete = function(group_id) {
            statement <-
                "DELETE FROM groups
                WHERE
                    group_id = {group_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
        },
        
        #' @description Edit group metadata
        group_edit = function(group_id, group_title, group_description) {
            statement <-
                "UPDATE groups
                SET
                    group_title = {group_title},
                    group_description = {group_description}
                WHERE
                    group_id = {group_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("Edited group '{group_id}'")
        },
        #' @description Add an user to a group
        group_user_add = function(group_id, user_id, user_color = "white", group_role = "user") {
            statement <-
                "INSERT INTO group_users
                SET
                    group_id = {group_id},
                    user_id = {user_id},
                    user_color = {user_color},
                    group_role = {group_role}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' inserted into group '{group_id}' with role '{group_role}'")
        },
        #' @description Delete an user from a group
        group_user_delete = function(group_id, user_id) {
            statement <-
                "DELETE FROM group_users
                WHERE
                    group_id = {group_id} AND
                    user_id = {user_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' deleted from group '{group_id}'")
        },

        #' @description Edit the role of a user inside a group and related information
        group_user_edit = function(group_id, user_id, user_color, group_role) {
            
            statement <-
                "UPDATE group_users
                SET
                    user_color = {user_color},
                    group_role = {group_role}
                WHERE
                    group_id = {group_id} AND
                    user_id = {user_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' now has role '{group_role}' and color '{user_color}' in group '{group_id}'")
        },
        
        #' @description Select a group for use in the board
        group_select = function(group_id) {
            self$group_selected <- group_id
        },
        
        #' @description Set selected group as favorite in the database
        group_set_as_favorite = function() {
            statement_set_new_favorite <- glue::glue_sql(
                "UPDATE group_users
                    SET 
                        favorite_group = TRUE
                    WHERE 
                        user_id = {user_id} AND
                        group_id = {group_id}
                ",
                .con = private$con,
                user_id = self$user$user_id,
                group_id = self$group_selected
            )
            
            statement_forget_old_favorite <- glue::glue_sql(
                "UPDATE group_users
                    SET 
                        favorite_group = FALSE
                    WHERE
                        user_id = {user_id} AND
                        group_id != {group_id}
                ",
                .con = private$con,
                user_id = self$user$user_id,
                group_id = self$group_selected
            )
            
            DBI::dbBegin(private$con)
            DBI::dbExecute(private$con, statement_set_new_favorite)
            DBI::dbExecute(private$con, statement_forget_old_favorite)
            DBI::dbCommit(private$con)
        },
        
        #' @description Add a measurement unit for a group
        group_unit_add = function(unit_title, unit_description = "", unit_type, unit_icon = 'file') {
            unit_id <- ids::random_id()
            unit_type <- match.arg(unit_type, c("report", "task"))
            
            statement <- "
                INSERT INTO units
                SET
                    group_id = {self$group_selected},
                    unit_id = {unit_id},
                    unit_title = {unit_title},
                    unit_description = {unit_description},
                    type = {unit_type},
                    icon = {unit_icon},
                    creator = {self$user$user_id},
                    last_modified_by = {self$user$user_id}
            "
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) cli::cli_alert_info("Inserted unit '{unit_id}' into group '{self$group_selected}'")
        },
        
        #' @description Edit a measurement unit from a group
        group_unit_edit = function(unit_id, unit_title, unit_description, unit_type, unit_icon) {
            unit_type <- match.arg(unit_type, c("report", "task"))
            
            statement <- "
                UPDATE units
                SET 
                    unit_title = {unit_title},
                    unit_description = {unit_description},
                    type = {unit_type},
                    icon = {unit_icon},
                    last_modified_by = {self$user$user_id}
                WHERE
                    unit_id = {unit_id}
            "
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) cli::cli_alert_info("Edited unit '{unit_id}' from '{self$group_selected}'")
        },
        
        #' @description Delete a measurement unit from a group
        group_unit_delete = function(unit_id) {
            super$db_execute_statement(
                "DELETE FROM units
                WHERE unit_id = {unit_id}",
                .envir = rlang::current_env()
            )
            
            if (interactive()) cli::cli_alert_info("Deleted unit '{unit_id}' from group '{self$group_selected}'")
        }
    ),
    private = list(
        get_groups = function() {

            query <-
                "SELECT
                    rhs.org_id, lhs.group_id, lhs.group_role,
                    rhs.group_title, rhs.group_description,
                    rhs.parent_group, rhs.time_creation, rhs.time_last_modified
                FROM (
                    SELECT group_id, group_role
                    FROM group_users
                    WHERE user_id = {self$user$user_id}
                ) lhs
                LEFT JOIN groups rhs ON
                    lhs.group_id = rhs.group_id"

            db_data <- super$db_get_query(query)


            db_data |>
                purrr::pmap(list) |>
                setNames(nm = db_data$group_id)
        },
        get_group_users = function() {
            query <-
                "SELECT
                    rhs.*,
                    rhs2.name, rhs2.last_name
                FROM (
                    SELECT group_id
                    FROM group_users
                    WHERE user_id = {self$user$user_id}
                ) lhs
                LEFT JOIN group_users rhs ON
                    lhs.group_id = rhs.group_id
                LEFT JOIN users rhs2 ON
                    rhs.user_id = rhs2.user_id
                ORDER BY rhs.group_role
                "

            db_data <- super$db_get_query(query)


            db_data |>
                split(~group_id) |>
                purrr::map(
                    ~purrr::pmap(.x, list) |>
                        setNames(.x$user_id)
                )
        },
        group_add_default_units = function() {
            defaults <- c("Informe", "Proyecto de informe", "Proyecto de oficio", 
                          "Proyecto de memorando", "Ayuda memoria", "PPT", 
                          "Entregable", "Correo")
            
            data <- data.frame(
                group_id = self$group_selected,
                unit_id = random_id(n = length(defaults)),
                unit_title = defaults,
                unit_description = "",
                type = "task",
                icon = "file",
                creator = self$user$user_id,
                last_modified_by = self$user$user_id
            ) |> 
                transform(
                    value = glue::glue_sql(
                        "({group_id}, {unit_id}, {unit_title}, {unit_description}, 
                        {type}, {icon}, {creator}, {last_modified_by})",
                        .con = private$con
                    )
                )
            
            values <- glue::glue_sql_collapse(data$value, sep = ",\n")
            
            statement <- "
                INSERT INTO 
                    units(group_id, unit_id, unit_title, unit_description,
                    type, icon, creator, last_modified_by)
                VALUES
                    {values}                    
            "
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            cli::cli_alert_info("Inserted default measurement units into group '{self$group_selected}'")
        }
    ),
    active = list(
        #' @field groups List containing the group affiliations of the User
        groups = function() {
            private$get_groups()
        },

        #' @field group_users List containing the user list of the group The info is shown following the User's group role.
        group_users = function() {
            private$get_group_users()
        },
        
        #' @field group_units List containing the group's measurement units. Older units are shown first.
        group_units = function() {
            data <- super$db_get_query("
                SELECT 
                    lhs.* ,
                    rhs1.name AS creator_name,
                    rhs1.last_name AS creator_last_name,
                    rhs2.name AS last_modifier_name,
                    rhs2.last_name AS last_modifier_last_name
                FROM (
                    SELECT * 
                    FROM units 
                    WHERE group_id = {self$group_selected}
                ) lhs
                LEFT JOIN users rhs1 ON
                    lhs.creator = rhs1.user_id
                LEFT JOIN users rhs2 ON
                    lhs.last_modified_by = rhs2.user_id
                ORDER BY time_creation
            ", .envir = rlang::current_env())
            
            data |> 
                purrr::pmap(list) |> 
                setNames(data$unit_id)
        }
    )
)
