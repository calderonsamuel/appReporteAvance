AppData <- R6::R6Class(
    classname = "AppData",
    inherit = Task,
    public = list(
        initialize = function(email) {
            super$initialize(email)
        }
    )
)
