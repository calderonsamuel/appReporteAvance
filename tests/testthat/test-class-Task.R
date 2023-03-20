test_that("Task starts as expected", {
    test_task <- Task$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    test_task |> 
        expect_type("environment") |> 
        expect_s3_class("R6") |> 
        expect_s3_class("Task")
    
})

test_that("Task can be created, edited and deleted", {
    test_task <- Task$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    group_id <- test_task$group_add(
        org_id = Sys.getenv("REPORTES_API_TEST_ORG"),
        group_title = "testing group",
        group_description = ""
    )
    
    expect_equal({
        test_id <- test_task$task_add(
            group_id = group_id,
            task_title = "testing task",
            task_description = "",
            assignee = test_task$user$user_id,
            time_due = Sys.Date() + lubridate::days(7),
            output_unit = "Informe",
            output_goal = 1L
        )
        
        test_task$task_edit_metadata(
            task_id = test_id,
            task_title = "Nuevo título",
            task_description = "Nueva descripción"
        )
        
        test_task$task_delete(task_id = test_id)
        
        "ok"
    }, expected = "ok")
    
    test_task$group_delete(group_id)
})

test_that("Tasks data can be retrieved", {
    test_task <- Task$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    expect_type(test_task$tasks, "list")
    expect_named(test_task$tasks)
})

test_that("Progress can be reported and produce history", {
    test_task <- Task$new(email = Sys.getenv("REPORTES_EMAIL"))
    
    group_id <- test_task$group_add(
        org_id = Sys.getenv("REPORTES_API_TEST_ORG"),
        group_title = "testing group",
        group_description = ""
    )
    
    test_id <- test_task$task_add(
        group_id = group_id,
        task_title = "testing task",
        task_description = "",
        assignee = test_task$user$user_id,
        time_due = Sys.Date() + lubridate::days(7),
        output_unit = "Informe",
        output_goal = 1L
    )
    
    expect_equal({
        test_task$task_report_progress(
            task_id = test_id,
            status_current = "En revisión",
            output_current = 1L,
            details = "Testing progress report"
        )
        "ok"
    }, expected = "ok")
    
    task_history <- test_task$task_get_history(test_id)
    
    expect_type(task_history, "list")
    expect_s3_class(task_history, "data.frame")
    
    test_task$task_delete(task_id = test_id)
    test_task$group_delete(group_id)
})
