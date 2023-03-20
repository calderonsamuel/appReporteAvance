test_that("User initializes as expected", {
    test_User <- User$new(Sys.getenv("REPORTES_EMAIL"))
    
    test_User |>
        expect_type("environment") |>
        expect_s3_class("R6") |>
        expect_s3_class("User")
})

test_that("User allows to add, edit, and delete users", {
    test_User <- User$new(Sys.getenv("REPORTES_EMAIL"))
    
    expect_equal({
        test_id <- test_User$user_add(
            name = "Ejemplo", 
            last_name = "Ejemplo",
            email = "testing@ejemplo.com"
        )
        
        test_User$user_edit_names(
            user_id = test_id,
            name = "New name",
            last_name = "New last name"
        )
        
        test_User$user_delete(user_id = test_id)
        
        "ok"
    }, expected = "ok")
})
