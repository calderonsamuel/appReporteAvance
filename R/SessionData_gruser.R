SessionData$set("public", "gruser_insert", function(field_list) {
    DBI::dbWriteTable(private$con, "group_users", field_list, append = TRUE)
    glue::glue("inserted user with id {user_id} into {group_id}",
               user_id = field_list$user_id,
               group_id = field_list$group_id) |> message()
})

SessionData$set("public", "gruser_remove", function(group_id, user_id) {
    statement <- "DELETE FROM group_users
              WHERE (user_id = {user_id}) AND (group_id = {group_id})"
    private$db_execute_statement(statement, user_id = user_id, group_id = group_id)
    glue::glue("deleted group user with id {user_id} from {group_id}") |> message()
})

SessionData$set("public", "gruser_get_groups", function() {
    query <- "SELECT *
            FROM group_users
            WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = self$user_id)
    return(data$group_id) # return a chr vector
})

SessionData$set("public", "gruser_get_from_group", function(group_id) {
    query <- "SELECT *
            FROM group_users
            WHERE (group_id IN ({vals*}))"
    data <- private$db_get_query(query, vals = group_id)
    return(sort(data$user_id)) # return a chr vector
})

SessionData$set("public", "gruser_get_metadata", function(group_id) {
    user_id_list <- self$gruser_get_from_group(group_id)
    self$user_get_names(user_id_list)
})

SessionData$set("public", "gruser_purge", function(user_id) {
    statement <- "DELETE FROM group_users
                  WHERE (user_id = {user_id})"
    private$db_execute_statement(statement, user_id = user_id)
    glue::glue("deleted group user with id {user_id}") |> message()
})

# testing

# test <- SessionData$new("dgco93@mininter.gob.pe")
# 
# test$gruser_insert(data.frame(
#     group_id = "team-ejemplo",
#     user_id = "testing_user"
# ))
# 
# test$gruser_remove("team-ejemplo", "testing_user")
# 
# rm(test)
# gc()

