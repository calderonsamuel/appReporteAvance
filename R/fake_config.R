fake_config <- function(AppData) {
    org <- AppData$orgs[[1]]$org_id
    group <- get_group_choices(AppData, org)[1] |> unname()
    list(
        org_selected = reactive(org),
        group_selected = reactive(group),
        group_colors_modified = reactive(1L),
        is_admin = reactive(verify_group_admin(AppData, group))
    )
}

