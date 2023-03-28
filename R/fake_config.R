fake_config <- function(app_data) {
    org <- app_data$orgs[[1]]$org_id
    group <- get_group_choices(app_data, org)[1] |> unname()
    list(
        org_selected = reactive(org),
        group_selected = reactive(group),
        group_colors_modified = reactive(1L),
        is_admin = reactive(verify_group_admin(app_data, group))
    )
}

