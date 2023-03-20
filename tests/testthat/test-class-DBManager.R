test_that("DBManager connects", {
    man <- DBManager$new() |> suppressMessages()
    expect_type(man, "environment")
    expect_s3_class(man, "R6")
    expect_s3_class(man, "DBManager")
})
