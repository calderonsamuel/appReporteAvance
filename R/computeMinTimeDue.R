computeMinTimeDue <- function(maxHour = 18L, tzone = "") {
    target <- lubridate::now(tzone)
    nowHour <- lubridate::hour(target)
    
    lubridate::hour(target) <- maxHour
    lubridate::minute(target) <- 0L
    lubridate::second(target) <- 0L
    
    if (nowHour >= maxHour) {
        target <- target + lubridate::days(1)
    }
    
    target
}

computeMinDateDue <- function(maxHour = 18L, tzone = "") {
    computeMinTimeDue(maxHour, tzone) |> lubridate::as_date()
}
