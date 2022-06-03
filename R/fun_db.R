library(RSQLite)

# REPORTE ----

ensure_reporte <- function() {
  if(!dbCanConnect(SQLite(), "reporte_avance.db")) print("created reporte_avance.db")
  con <- dbConnect(SQLite(), "reporte_avance.db")
  dbDisconnect(con)
}

ensure_reporte()




## TASKS ----

create_reporte_tasks <- function() {
  con <- dbConnect(SQLite(), "reporte_avance.db")

  if (!dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = character(),
      user = character(),
      task_id = character(),
      unique_id = character()
    )

    dbWriteTable(con, "tasks", task_list)
    print("created table 'tasks'")
  }

  dbDisconnect(con)
}

get_tasks <- function() {
  con <- dbConnect(SQLite(), "reporte_avance.db")
  data <- dbReadTable(con, "tasks")
  dbDisconnect(con)
  return(data)
}

insert_task <- function(task_list) {
  con <- dbConnect(SQLite(), "reporte_avance.db")
  dbWriteTable(con, "tasks", task_list, append = TRUE)
  dbDisconnect(con)
  print(sprintf("inserted task with id %s", task_list$unique_id))
}

delete_task <- function(unique_id) {
  con <- dbConnect(SQLite(), "reporte_avance.db")
  dbExecute(con, sprintf("DELETE FROM tasks WHERE (unique_id = '%s')", unique_id))
  dbDisconnect(con)
  print(sprintf("deleted task with id %s", unique_id))
}

create_reporte_tasks()
get_tasks()

task_1 <- data.frame(
  reviewer = "dgco93",
  user = "dgco90",
  task_id = "AOI001",
  unique_id = "0905002"
)

insert_task(task_1)
get_tasks()
delete_task("0905002")
get_tasks()

## USERS ----

create_reporte_users <- function() {
  con <- dbConnect(SQLite(), "reporte_avance.db")

  if (!dbExistsTable(con, "users")) {
    field_list <- data.frame(
      user = character(),
      name = character(),
      last_name = character(),
      contract_type = character()
    )

    dbWriteTable(con, "users", field_list)
    print("created table 'users'")
  }

  dbDisconnect(con)
}

insert_user <- function(user_data) {
  con <- dbConnect(SQLite(), "reporte_avance.db")
  dbWriteTable(con, "users", user_data, append = TRUE)
  dbDisconnect(con)
}

get_users <- function() {
  con <- dbConnect(SQLite(), "reporte_avance.db")
  data <- dbReadTable(con, "users")
  dbDisconnect(con)
  return(data)
}




users <- data.frame(
  user = c("dgco93", "dgco26", "dgco84"),
  name = c("Samuel", "Miguel", "Leanna"),
  last_name =c("Calderon", "Rojas", "Zuñiga"),
  contract_type = c("OS", "OS", "OS")
)

insert_user(users)
get_users()
