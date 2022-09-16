library(promises)
library(future)

plan(multisession)

placeholder <- NULL

test <- future_promise({
    Sys.sleep(0.5)
    sample(letters, 10)
    }) %...>%
    { placeholder <<- . }

placeholder

## Conclusion: Doable when working with a parallel backend package
