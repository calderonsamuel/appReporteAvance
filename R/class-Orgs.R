#' Get Organisation data
#'
#' R6 class that allows to get the Organisation information.
#'
#' @param email The email the user started the session with.
#' @param org_id The id of the organisation on which the statement will be executed
#' @param user_id The id of the user on which the statement will be executed
#' @param org_title The new title of the organisation
#' @param org_description The new description of the organisation
#' @param org_role The role for the user in the organisation
Organisation <- R6::R6Class(
    classname = "Organisation",
    inherit = User,
    public = list(
        #' @description Start an Organisation based on an user email
        initialize = function(email) {
            super$initialize(email)
        },
        #' @description Initialize an organisation for a new user
        org_initialize = function() {
            org_id <- self$org_add("Organización sin nombre", "")

            self$org_user_add(
                org_id = org_id,
                user_id = self$user$user_id,
                org_role = "owner"
            )

            if (interactive()) cli::cli_alert_info("Initialized org '{org_id}'")
        },
        #' @description Add a new organisation to the database
        org_add = function(org_title, org_description) {
            id <- ids::random_id()
            
            statement <-
                "INSERT INTO organisations
                SET
                    org_id = {id},
                    org_title = {org_title},
                    org_description = {org_description}"
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            if (interactive()) cli::cli_alert_info("Created org '{id}'")
            
            return(id)
        },
        #' @description Delete an organisation from the database
        org_delete = function(org_id) {
            statement <-
                "DELETE FROM organisations
                WHERE org_id = {org_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            if (interactive()) cli::cli_alert_info("Deleted org '{org_id}'")
        },
        #' @description Edit Organisation metadata
        org_edit = function(org_id, org_title, org_description) {
            statement <-
                "UPDATE organisations
                SET
                    org_title = {org_title},
                    org_description = {org_description}
                WHERE
                    org_id = {org_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("Edited org '{org_id}'")
        },
        
        #' @description Add an user to an organisation
        org_user_add = function(org_id, user_id, org_role) {
            statement <-
                "INSERT INTO org_users
                SET
                    org_id = {org_id},
                    user_id = {user_id},
                    org_role = {org_role}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' inserted into org '{org_id}' with role '{org_role}'")
        },
        
        #' @description Delete an user from an organisation
        org_user_delete = function(org_id, user_id) {
            statement <-
                "DELETE FROM org_users
                WHERE
                    org_id = {org_id} AND
                    user_id = {user_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' deleted from org '{org_id}'")
        },

        #' @description Edit the role of a user inside an organisation
        org_user_edit = function(org_id, user_id, org_role) {
            statement <-
                "UPDATE org_users
                SET
                    org_role = {org_role}
                WHERE
                    org_id = {org_id} AND
                    user_id = {user_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            if (interactive()) cli::cli_alert_info("User '{user_id}' now has role '{org_role}' in org '{org_id}'")
        },


        #' @description Remove the existence of an organisation
        org_finalize = function(org_id) {
            statement <-
                "DELETE FROM org_users
                WHERE org_id = {org_id}"

            super$db_execute_statement(statement, .envir = rlang::current_env())

            private$org_delete(org_id)

            if (interactive()) cli::cli_alert_info("Finalized org '{org_id}'")
        }
    ),
    private = list(
        get_orgs = function() {

            query <-
                "SELECT
                    lhs.org_id, lhs.org_role,
                    rhs.org_title, rhs.org_description,
                    rhs.time_creation, rhs.time_last_modified
                FROM (
                    SELECT org_id, org_role
                    FROM org_users
                    WHERE user_id = {self$user$user_id}
                ) lhs
                LEFT JOIN organisations rhs ON
                    lhs.org_id = rhs.org_id"

            db_data <- super$db_get_query(query)


            db_data |>
                purrr::pmap(list) |>
                setNames(nm = db_data$org_id)
        },
        get_org_users = function() {
            query <-
                "SELECT
                    rhs.*,
                    rhs2.name, rhs2.last_name
                FROM (
                    SELECT org_id
                    FROM org_users
                    WHERE user_id = {self$user$user_id}
                ) lhs
                LEFT JOIN org_users rhs ON
                    lhs.org_id = rhs.org_id
                LEFT JOIN users rhs2 ON
                    rhs.user_id = rhs2.user_id
                ORDER BY rhs.org_role
                "

            db_data <- super$db_get_query(query)


            db_data |>
                split(~org_id) |>
                purrr::map(
                    ~purrr::pmap(.x, list) |>
                        setNames(.x$user_id)
                )
        }
    ),
    active = list(
        #' @field orgs List containing the organisation affiliations of the User
        orgs = function() {
            private$get_orgs()
        },
        #' @field org_users List containing the user list of the organisation. The info is shown following the User's organisation role.
        org_users = function() {
            private$get_org_users()
        }
    )
)
