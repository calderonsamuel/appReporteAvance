library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)

mtcars2 <- tbl(con, "mtcars")
mtcars2

mtcars2 |>
  filter(cyl %in% c("6", "4")) |>
  show_query()
