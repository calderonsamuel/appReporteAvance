boxHide <- function(box) {
    boxId <- boxFindId(box)

    box |>
        tagAppendAttributes(.cssSelector = glue::glue("#{boxId}"), style = "display: none;")
}

boxFindId <- function(box) {
    boxQuery <- htmltools::tagQuery(box)
    innerBox <- boxQuery$
        children(".card")$
        selectedTags()[[1]]

    tagGetAttribute(innerBox, "id")
}
