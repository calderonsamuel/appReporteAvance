test_that("group_get_members_list() returns list", {
  my_sd <- SessionData$new("dgco93@mininter.gob.pe")
  expect_type(my_sd$group_get_members_list("team-politicas"), "list")
})
