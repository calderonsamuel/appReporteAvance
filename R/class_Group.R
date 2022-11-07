Group <- R6::R6Class(
    classname = "Group",
    inherit = Organisation,
    public = list(
        groups = NULL,
        group_users = NULL,
        initialize = function(email) {
            super$initialize(email)
            private$sync_groups()
        },
        group_initialize = function() {
            group_id <- private$group_create()
            
            self$groip_user_add(
                org_id = org_id,
                group_id = group_id,
                user_id = self$user$user_id,
                role = "ownr"
            )
            
            private$sync_groups()
            
            cli::cli_alert_info("Initialized group '{group_id}' in org '{org_id}'")
        },
        group_edit = function(org_id, group_id, group_title, group_description) {
            t_stamp <- super$get_timestamp()
            statement <- 
                "UPDATE groups
                SET 
                    group_title = {group_title},
                    group_description = {group_description},
                    time_last_modified = {t_stamp}
                WHERE
                    org_id = {org_id} AND
                    group_id = {group_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            private$sync_groups()
            
            cli::cli_alert_info("Edited group '{group_id}' from org '{org_id}'")
        },
        group_user_add = function(org_id, group_id, user_id, role) {
            t_stamp <- super$get_timestamp()
            statement <- 
                "INSERT INTO org_users
                SET
                    org_id = {org_id},
                    group_id = {group_id},
                    user_id = {user_id},
                    group_role = {role},
                    time_creation = {t_stamp},
                    time_last_modified = {t_stamp}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            private$sync_group_users()
            
            cli::cli_alert_info("User '{user_id}' inserted into group '{group_id}' in org '{org_id}' with role '{role}'")
        },
        group_user_delete = function(org_id, group_id, user_id) {
            statement <- 
                "DELETE FROM group_users
                WHERE 
                    org_id = {org_id} AND
                    group_id = {group_id} AND
                    user_id = {user_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            private$sync_group_users()
            
            cli::cli_alert_info("User '{user_id}' deleted from group '{group} in org '{org_id}'")
        },
        
        group_user_edit = function() {
            
        },
        group_finalize = function(org_id, group_id) {
            statement <- 
                "DELETE FROM group_users
                WHERE 
                    org_id = {org_id} AND
                    group_id = {group_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            private$group_delete(org_id, group_id) # this syncs after
            
            cli::cli_alert_info("Finalized group '{group_id}' in org '{org_id}'")
        }
    ),
    private = list(
        get_groups = function() {
            
            query <-
                "SELECT 
                    lhs.org_id, lhs.group_id, lhs.group_role, 
                    rhs.group_title, rhs.group_description,
                    rhs.parent_group, rhs.time_creation, rhs.time_last_modified
                FROM ( 
                    SELECT org_id, group_id, group_role
                    FROM group_users
                    WHERE user_id = {self$user$user_id} 
                ) lhs
                LEFT JOIN groups rhs ON
                    lhs.org_id = rhs.org_id AND
                    lhs.group_id = rhs.group_id"
            
            db_data <- super$db_get_query(query)
            
            
            db_data |> 
                purrr::pmap(list) |> 
                setNames(nm = db_data$group_id)
        },
        get_group_users = function() {
            query <- 
                "SELECT
                    rhs.*
                FROM (
                    SELECT org_id, group_id
                    FROM group_users
                    WHERE user_id = {self$user$user_id} 
                ) lhs
                LEFT JOIN group_users rhs ON
                lhs.org_id = rhs.org_id AND
                    lhs.group_id = rhs.group_id"
            
            db_data <- super$db_get_query(query)
            
            
            db_data |> 
                split(~group_id) |> 
                purrr::map(
                    ~purrr::pmap(.x, list) |> 
                        setNames(.x$user_id)
                )
        },
        group_create = function(org_id, parent_group = "organisation") {
            group_id <- ids::random_id()
            t_stamp <- super$get_timestamp()
            
            statement <- 
                "INSERT INTO organisations
                SET
                    org_id = {org_id},
                    group_id = {group_id},
                    group_title = 'Grupo sin nombre',
                    group_description = '',
                    parent_group = {parent_group},
                    time_creation = {t_stamp},
                    time_last_modified = {t_stamp}"
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            return(group_id)
        },
        group_delete = function(org_id, group_id) {
            statement <- 
                "DELETE FROM groups
                WHERE 
                    org_id = {org_id} AND
                    group_id = {group_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            private$sync_groups()
        },
        sync_groups = function() {
            self$groups <- private$get_groups()
            self$group_users <- private$get_group_users()
        },
        sync_group_users = function() {
            self$group_users <- private$get_group_users()
        }
    )
)
