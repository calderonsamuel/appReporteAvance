fake_config <- function(AppData) {
    org <- AppData$orgs[[1]]$org_id
    group <- get_group_choices(AppData, org)[1] |> unname()
    list(
        org_selected = reactive(org),
        group_selected = reactive(group)
    )
}

