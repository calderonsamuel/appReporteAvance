#' Error on missing environment variable by name
#'
#' The function searches for the given name as an environment variable and returns its value.
#' If the value is not found, an error message is generated.
#'
#' @param name A character string indicating the name of the environment variable
#' @return If the environment variable is found, its value is returned and quotes are removed. Otherwise, an error message is generated.
#' @examples
#' error_on_missing_name("PATH")
#' error_on_missing_name("NONEXISTENT")
#' error_on_missing_name("")
error_on_missing_name <- function(name){
    var <- Sys.getenv(name, unset=NA)
    if(is.na(var)){
        stop(paste0("cannot find ",name, " !"),call. = FALSE)
    }
    gsub("\"", '',var)
}

# Authenticate
rsconnect::setAccountInfo(
    name = error_on_missing_name("SHINYAPPSIO_ACC_NAME"),
    token = error_on_missing_name("SHINYAPPSIO_TOKEN"),
    secret = error_on_missing_name("SHINYAPPSIO_SECRET")
)
# Deploy the application.
rsconnect::deployApp(appName = error_on_missing_name("SHINYAPPSIO_MASTERNAME")) # will be used in the url
