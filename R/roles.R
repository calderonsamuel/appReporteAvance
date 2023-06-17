load_roles <- function() {
    path_group <- system.file(package = methods::getPackageName(), "data-raw/roles - group.csv")
    roles_group <- readr::read_csv(path_group) |> suppressMessages()
    
    list(
        group = purrr::pmap(roles_group, list) |> setNames(roles_group$role)
    )
}
