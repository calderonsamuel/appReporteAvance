Group <- R6::R6Class(
    classname = "Group",
    inherit = Organisation,
    public = list(
        groups = NULL,
        group_users = NULL,
        initialize = function(email) {
            super$initialize(email)
            self$groups <- private$get_groups()
            self$group_users <- private$get_group_users()
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
        }
    )
)
