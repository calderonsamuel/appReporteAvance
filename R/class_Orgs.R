Organisation <- R6::R6Class(
    classname = "Organisation",
    inherit = User,
    public = list(
        orgs = NULL,
        org_users = NULL,
        initialize = function(email) {
            super$initialize(email)
            self$orgs <- private$get_orgs()
            self$org_users <- private$get_org_users()
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
        }
    )
)
