#' Hide element for firebase UI
#'
#' This is a wrapper for firebase::reqSignin.
#' Real head scratcher of a bug!!
#'
#' @param ... taglist
#'
#' @noRd
#'
reqSignin <- function (...)
{
  tagAppendAttributes(...,
                      class ="fireblaze__requires__signin fireblaze__hidden",
                      .cssSelector = ".wrapper")
}
