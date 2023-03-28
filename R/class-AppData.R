#' Get App data
#'
#' R6 class that allows to get the information needed for an User session of the app.
#'
#' @param email The email the user started the session with.
#' @export
AppData <- R6::R6Class(
    classname = "AppData",
    inherit = Task,
    public = list(
        #' @description Start a session of app data based on an user email
        initialize = function(email = Sys.getenv("REPORTES_EMAIL")) {
            super$initialize(email)
        }
    )
)
