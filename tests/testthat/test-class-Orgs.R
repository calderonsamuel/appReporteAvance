test_that("Org starts as expected", {
    test_Orgs <- Organisation$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    test_Orgs |> 
        expect_type("environment") |> 
        expect_s3_class("R6") |> 
        expect_s3_class("Organisation")
    
})

test_that("Org can be created, edited and deleted", {
    test_Orgs <- Organisation$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_equal({
        test_id <- test_Orgs$org_add("Test", "")
        
        test_Orgs$org_edit(
            org_id = test_id,
            org_title = "Nuevo título",
            org_description = "Nueva descripción"
        )
        
        test_Orgs$org_delete(org_id = test_id)
        
        "ok"
    }, expected = "ok")
})

test_that("Org data can be retrieved", {
    test_Orgs <- Organisation$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_type(test_Orgs$orgs, "list")
    expect_named(test_Orgs$orgs)
})

test_that("Org user can be added and edited", {
    test_Orgs <- Organisation$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    test_org_id <- test_Orgs$org_add("Test", "")
    
    expect_equal({
        test_Orgs$org_user_add(
            org_id = test_org_id,
            user_id = test_Orgs$user$user_id,
            org_role = "user"
        )
        
        test_Orgs$org_user_edit(
            org_id = test_org_id,
            user_id = test_Orgs$user$user_id,
            org_role = "admin"
        )
        
        test_Orgs$org_user_delete(
            org_id = test_org_id,
            user_id = test_Orgs$user$user_id
        )
        
        test_Orgs$org_delete(org_id = test_org_id)
        
        "ok"
    }, expected = "ok")
})

test_that("Org users data can be retrieved", {
    test_Orgs <- Organisation$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_type(test_Orgs$org_users, "list")
    expect_named(test_Orgs$org_users)
})
