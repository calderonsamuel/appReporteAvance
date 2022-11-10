library(tidyverse)
library(lubridate)
library(ids)

con_prod <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT")
)


## Data recuperation ----

users <- DBI::dbGetQuery(con_prod, "SELECT * FROM users")
tasks <- DBI::dbGetQuery(con_prod, "SELECT * FROM tasks")
progress <- DBI::dbGetQuery(con_prod, "SELECT * FROM progress")
groups <- DBI::dbGetQuery(con_prod, "SELECT * FROM groups")

DBI::dbDisconnect(con_prod)

cli::cli_alert_success("Data retrieved")

## Constructors ----

create_organisation <- function(org_title, org_description) {
    tibble(
        org_id = random_id(),
        org_title = org_title,
        org_description = org_description,
        time_creation = now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_user <- function(name, last_name, email, time_creation = NULL) {
    tibble(
        user_id = random_id(),
        name = name,
        last_name = last_name,
        email = email,
        time_creation = time_creation %||% now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_org_user <- function(org_id, user_id, org_role) {
    tibble(
        org_id = org_id,
        user_id = user_id,
        org_role = org_role,
        time_creation = now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_group <- function(org_id, group_title, group_description, parent_group) {
    tibble(
        org_id = org_id,
        group_id = random_id(),
        group_title = group_title,
        group_description = group_description,
        parent_group = parent_group,
        time_creation = now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_group_user <- function(org_id, group_id, user_id, user_color, group_role) {
    tibble(
        org_id = org_id,
        group_id = group_id,
        user_id = user_id,
        user_color = user_color,
        group_role = group_role,
        time_creation = now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_task <- function(org_id, group_id, task_title, 
                        task_description, assigned_by, assignee, time_due,
                        output_unit, output_goal, output_current, 
                        status_current = NULL,
                        process_id = NA_character_, activity_id = NA_character_,
                        time_creation = NULL) {
    tibble(
        process_id = process_id,
        activity_id = activity_id,
        org_id = org_id,
        group_id = group_id,
        task_id = random_id(),
        task_title = task_title,
        task_description = task_description,
        assigned_by = assigned_by,
        assignee = assignee,
        time_due = time_due,
        output_unit = output_unit,
        output_goal = output_goal,
        output_current = output_current,
        status_current = status_current %||% "Pendiente",
        time_creation = time_creation %||% now("America/Lima"),
        time_last_modified = now("America/Lima")
    )
}

create_progress <- function(org_id, group_id, task_id, 
                            reported_by, output_progress, status, details,
                            process_id = NA_character_, activity_id = NA_character_,
                            time_reported = NULL) {
    tibble(
        process_id = process_id,
        activity_id = activity_id,
        org_id = org_id,
        group_id = task_id,
        task_id = task_id,
        reported_by = reported_by,
        output_progress = output_progress,
        status = status,
        time_reported = time_reported %||% now("America/Lima"),
        details = details
    )
}


## DB preparation ----

db_organisations <- create_organisation("Ministerio del Interior", "Ministerio del Interior de Perú")

db_users <- 
    users |> 
        select(name, last_name, email = user_id, time_creation = date_added) |> 
        mutate(time_creation = as_datetime(time_creation)) |> 
        pmap_dfr(create_user) |> 
        add_row(create_user("Miguel", "Rojas", "dgco26@mininter.gob.pe"))

db_org_users <-
    db_users |> 
        select(user_id) |> 
        mutate(org_id = db_organisations$org_id, org_role = "user") |> 
        pmap_dfr(create_org_user)


db_groups <-
    groups |> 
        mutate(
            org_id = db_organisations$org_id,
            parent_group = "organisation"
        ) |> 
        rename(group_title = group_id) |> 
        select(-group_admin) |> 
        pmap_dfr(create_group) |> 
        add_row(create_group(db_organisations$org_id, "Main", "Main wrapper", "organisation"))
        

db_group_users <- db_groups |> 
    select(ends_with("_id")) |> 
    left_join(db_org_users) |> 
    select(-starts_with("time")) |>
    rename(group_role = org_role) |> 
    group_by(group_id) |> 
    mutate(user_color = sample(bs4Dash::getAdminLTEColors(), size = n())) |> 
    ungroup() |> 
    pmap_dfr(create_group_user) 



get_new_id_user <- function(old_id) {
    db_users |> 
        filter(email == old_id) |> 
        slice_head(n = 1) |> 
        pull(user_id)
} 

get_new_id_group <- function(old_id) {
    db_groups |> 
        filter(group_title == old_id) |> 
        slice(1) |> 
        pull(group_id)
}

get_new_id_task <- function(old_id) {
    db_tasks |> 
        filter(task_description == old_id) |> 
        slice(1) |> 
        pull(task_id)
}

get_task_time_started <- function(task_id_stored) {
    progress |> 
        filter(task_id == task_id_stored) |> 
        slice(1) |> 
        pull(time) |> 
        as_datetime()
}


db_tasks <- 
    tasks |> 
    as_tibble() |> 
    filter(!str_starts(user_id, "team")) |> 
    rename(status_current = status, task_title = task_description) |> 
    mutate(
        assigned_by = map_chr(reviewer, get_new_id_user),
        assignee = map_chr(user_id, get_new_id_user),
        task_description = task_id
    ) |> 
    select(-c(user_id, reviewer, template_id, task_id)) |> 
    mutate(
        process_id = NA_character_,
        activity_id = NA_character_,
        org_id = db_organisations$org_id,
        group_id = get_new_id_group("team-politicas"),
        time_due = now("America/Lima") + weeks(2),
        output_unit = "Documento",
        output_goal = 1L,
        output_current = if_else(status_current == "Terminado", 1L, 0L)
    ) |> 
    pmap_dfr(create_task)

db_progress <- progress |>
    as_tibble() |> 
    semi_join(db_tasks, by = c("task_id" = "task_description")) |> 
    mutate(
        process_id = NA_character_,
        activity_id = NA_character_,
        org_id = db_organisations$org_id,
        group_id = get_new_id_group("team-politicas"),
        task_id = map_chr(task_id, get_new_id_task),
        reported_by = map_chr(reported_by, get_new_id_user),
        time = as_datetime(time),
        output_progress = if_else(status == "Terminado", 1L, 0L)
    ) |> 
    rename(details = explain) |> 
    select(-c(status_id, step_id)) |> 
    pmap_dfr(create_progress)

cli::cli_alert_success("Data processed")
   
## Data migration ----

# This needs to change for a remote DB 
con_dev <- DBI::dbConnect(
    # RSQLite::SQLite(), "inst/scripts/db_v0-3-0.db"
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME_DEV"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT")
)

xrep <- \(num = 32) strrep("x", num)

base_users <- create_user(
    name = xrep(32),
    last_name = xrep(32),
    email = xrep(64)
)

base_organisations <-
    create_organisation(
        org_title = xrep(64), 
        org_description = xrep(256)
    )    

base_org_users <- 
    create_org_user(
        org_id = ids::random_id(),
        user_id = ids::random_id(), 
        org_role = xrep(32))

base_groups <- 
    create_group(
        org_id = ids::random_id(),
        group_title = xrep(64), 
        group_description = xrep(256), 
        parent_group = ids::random_id()
    )

base_group_users <- 
    create_group_user(
        org_id = ids::random_id(), 
        group_id = ids::random_id(), 
        user_id = ids::random_id(), 
        user_color = xrep(32),
        group_role = xrep(32)
    )

base_tasks <- 
    create_task(
        org_id = ids::random_id(), 
        group_id = ids::random_id(), 
        task_title = xrep(256),
        task_description = xrep(512), 
        assigned_by = ids::random_id(), 
        assignee = ids::random_id(), 
        time_due = lubridate::now("America/Lima"), 
        output_unit = xrep(32),
        output_goal = 9999999.99, 
        output_current = 9999999.99,
        status_current = xrep(32), 
        process_id = ids::random_id(), 
        activity_id = ids::random_id()
    )

base_progress <- 
    create_progress(
        org_id = ids::random_id(), 
        group_id = ids::random_id(),
        task_id = ids::random_id(), 
        reported_by = ids::random_id(), 
        output_progress = 9999999.99, 
        status = xrep(32), 
        details = xrep(256), 
        process_id = ids::random_id(), 
        activity_id = ids::random_id()
    )

base_df <- list(
    users = base_users,
    organisations = base_organisations,
    org_users = base_org_users,
    groups = base_groups,
    group_users = base_group_users,
    tasks = base_tasks,
    progress = base_progress
)

cli::cli_alert_info("Dummy tables created")

# Set placeholders
base_df |> 
    names() |> 
    walk(~DBI::dbWriteTable(con_dev, name = .x, value = base_df[[.x]], overwrite = TRUE))

cli::cli_alert_info("Dummy tables inserted")

# Delete placeholders
base_df |> 
    names() |> 
    walk(~DBI::dbExecute(con_dev, paste0("DELETE FROM ", .x)))

cli::cli_alert_info("Dummy tables deleted")


# Put real data
df_list <- list(
    users = db_users,
    organisations = db_organisations,
    org_users = db_org_users,
    groups = db_groups,
    group_users = db_group_users,
    tasks = db_tasks,
    progress = db_progress
)

df_list |> 
    names() |> 
    walk(~DBI::dbWriteTable(con_dev, name = .x, value = df_list[[.x]], append = TRUE))

cli::cli_alert_info("Real data inserted")

DBI::dbListTables(con_dev)

DBI::dbDisconnect(con_dev)

cli::cli_alert_success("Data migrated")
