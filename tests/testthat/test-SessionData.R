test_that("SessionData starts with email", {
    my_sd <- SessionData$new("dgco93@mininter.gob.pe")
    expect_s3_class(my_sd, class = "R6")
    expect_s3_class(my_sd, class = "SessionData")
})

test_that("SessionData has expected initial values", {
    my_sd <- SessionData$new("dgco93@mininter.gob.pe")
    
    my_sd$groups |> 
        expect_type("list") |> 
        expect_s3_class(NA)
})

