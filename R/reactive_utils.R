reactive_counter <- function(input_add, input_rm,
                             start_value = 0L,
                             min_value = 0L,
                             max_value = Inf,
                             step_value = 1L) {
  step_count <- reactiveVal(start_value)

  observeEvent(input_add, {
    if (step_count() < max_value) {
      step_count(step_count() + step_value)
    }
  })

  observeEvent(input_rm,{
    if (step_count() > min_value) {
      step_count(step_count() - step_value)
    }
  })

  step_count
}
