test_that("Group initializes as expected", {
    test_group <- Group$new(Sys.getenv("REPORTES_EMAIL"))
    
    test_group |>
        expect_type("environment") |>
        expect_s3_class("R6") |>
        expect_s3_class("Group")
})

test_that("Group can be created, edited and deleted", {
    test_group <- Group$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_equal({
        test_id <- test_group$group_add(
            org_id = Sys.getenv("REPORTES_API_TEST_ORG"),
            group_title = "Test group",
            group_description = ""
        )
        
        test_group$group_edit(
            group_id = test_id,
            group_title = "Nuevo título",
            group_description = "Nueva descripción"
        )
        
        test_group$group_delete(group_id = test_id)
        
        "ok"
    }, expected = "ok")
})

test_that("Group data can be retrieved", {
    test_group <- Group$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_type(test_group$groups, "list")
    expect_named(test_group$groups)
})

test_that("Group data includes org_id", {
    test_group <- Group$new(email = Sys.getenv("REPORTES_EMAIL"))
    expect_type(test_group$groups[[1]]$org_id, "character")
})

test_that("Group users data can be retrieved", {
    test_group <- Group$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_type(test_group$group_users, "list")
    expect_named(test_group$group_users)
})

test_that("Group user can be added, edited and deleted", {
    test_group <- Group$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_equal({
        test_id <- test_group$group_add(
            org_id = Sys.getenv("REPORTES_API_TEST_ORG"),
            group_title = "Test group",
            group_description = ""
        )
        
        test_group$group_user_add(
            group_id = test_id,
            user_id = test_group$user$user_id,
            user_color = "white",
            group_role = "user"
        )
        
        test_group$group_user_edit(
            group_id = test_id,
            user_id = test_group$user$user_id,
            user_color = "black",
            group_role = "admin"
        )
        
        test_group$group_user_delete(
            group_id = test_id,
            user_id = test_group$user$user_id
        )
        
        test_group$group_delete(group_id = test_id)
        
        "ok"
    }, expected = "ok")
})

# TODO: Add testing cases for group_units
