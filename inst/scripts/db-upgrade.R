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

con_dev <- DBI::dbConnect(RSQLite::SQLite(), "inst/scripts/db_v0-3-0.db")

users <- DBI::dbGetQuery(con_prod, "SELECT * FROM users")
tasks <- DBI::dbGetQuery(con_prod, "SELECT * FROM tasks")
progress <- DBI::dbGetQuery(con_prod, "SELECT * FROM progress")

db_users <- 
    users |> 
    mutate(
        email = user_id,
        time_creation = as_datetime(date_added),
        time_last_modified = time_creation,
        user_id = random_id(6)
    ) |> 
    relocate(user_id) |> 
    as_tibble() |> 
    select(-date_added)


db_organisations <-
    tibble(
        org_id = random_id(),
        org_title = "Mininsterio del Interior",
        org_description = "Ministerio del Interior de Perú",
        time_creation = now(),
        time_last_modified = now()
    )


db_org_users <-
    db_users |> 
        bind_rows(db_organisations |> select(org_id)) |> 
        fill(org_id, .direction = "up") |> 
        mutate(user_org_id = random_id(n())) |> 
        select(org_id, user_org_id, user_id, time_creation, time_last_modified) |> 
        filter(!is.na(user_id)) |> 
        mutate(org_role = "user", .after = user_id)


db_groups <-
    db_organisations |> 
        select(org_id) |> 
        mutate(
            group_id = random_id(),
            group_title = "Main",
            group_description = "This group will contain all the members recently added until you move them to another group",
            parent_group = "organisation",
            time_creation = now(),
            time_last_modified = now()
        )

db_group_users <- db_groups |> 
    select(ends_with("_id")) |> 
    left_join(db_org_users) |> 
    select(-user_id) |> 
    rename(group_role = org_role)

tasks |> 
    relocate(
        task_id,
        task_title = task_description,
        assigned_by = reviewer, 
        assignee = user_id, 
        status_current = status
    ) |> 
    mutate(
        
    ) |> 
    names()


DBI::dbDisconnect(con_prod)
DBI::dbDisconnect(con_dev)
