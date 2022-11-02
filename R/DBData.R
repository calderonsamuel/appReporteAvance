DBData <- R6::R6Class(
    classname = "DBData",
    inherit = DBManager,
    public = list(
        user = NULL,
        orgs = NULL,
        org_users = NULL,
        groups = NULL,
        group_users = NULL,
        initialize = function(email) {
            super$initialize()
            self$user <- private$get_user_from_email(email)
            self$orgs <- private$get_orgs()
            self$groups <- private$get_groups()
            self$org_users <- private$get_org_users()
            self$group_users <- private$get_group_users()
        }
    ),
    private = list(
        get_user_from_email = function(email) {
            query <-
                "SELECT *
                FROM users
                WHERE email = {email}"
            
            data_returned <- super$db_get_query(query, email = email) # should be 1 row
            as.list(data_returned)
        },
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
        get_org_users = function() {
            
        },
        get_group_users = function() {
            query <- 
                "SELECT
                    rhs.*
                FROM (
                    SELECT org_id, group_id, group_role
                    FROM group_users
                    WHERE user_id = {self$user$user_id} 
                ) lhs
                LEFT JOIN group_users rhs ON
                lhs.org_id = rhs.org_id AND
                    lhs.group_id = rhs.group_id"
            
            db_data <- super$db_get_query(query)
            
            
            db_data |> 
                identity()
                # purrr::pmap(list) |> 
                # setNames(nm = db_data$group_id)
        }
    )
)
