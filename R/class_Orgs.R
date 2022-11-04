Organisation <- R6::R6Class(
    classname = "Organisation",
    inherit = User,
    public = list(
        orgs = NULL,
        org_users = NULL,
        initialize = function(email) {
            super$initialize(email)
            private$sync_orgs()
        },
        org_initialize = function() {
            org_id <- private$org_create()
            
            self$org_add_user(
                org_id = org_id,
                user_id = self$user$user_id,
                role = "owner"
            )
            private$sync_orgs()
        },
        org_add_user = function(org_id, user_id, role) {
            t_stamp <- super$get_timestamp()
            statement <- 
                "INSERT INTO org_users
                SET
                    org_id = {org_id},
                    user_id = {user_id},
                    role = {role},
                    time_creation = {t_stamp},
                    time_last_modofied = {t_stamp}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            private$sync_org_users()
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
                    rhs.*
                FROM (
                    SELECT org_id
                    FROM org_users
                    WHERE user_id = {self$user$user_id} 
                ) lhs
                LEFT JOIN org_users rhs ON
                lhs.org_id = rhs.org_id"
            
            db_data <- super$db_get_query(query)
            
            
            db_data |> 
                split(~org_id) |> 
                purrr::map(
                    ~purrr::pmap(.x, list) |> 
                        setNames(.x$user_id)
                )
        },
        org_create = function() {
            id <- ids::random_id()
            t_stamp <- super$get_timestamp()
            
            statement <- 
                "INSERT INTO organisations
                SET
                    org_id = {id},
                    org_title = 'Organización sin nombre',
                    org_description = '',
                    time_creation = {t_stamp},
                    time_last_modified = {t_stamp}"
            super$db_execute_statement(statement, .envir = rlang::current_env())
            
            return(id)
        },
        org_delete = function(org_id) {
            statement <- 
                "DELETE FROM organisations
                WHERE org_id = {org_id}"
            
            super$db_execute_statement(statement, .envir = rlang::current_env())
            private$sync_orgs()
        },
        sync_orgs = function() {
            self$orgs <- private$get_orgs()
            self$org_users <- private$get_org_users()
        },
        sync_org_users = function() {
            self$org_users <- private$get_org_users()
        }
    )
)
