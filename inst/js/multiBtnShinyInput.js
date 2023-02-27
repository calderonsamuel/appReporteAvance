$(document).on('click', ".multi-btn", function(event) {
    let shinyInputId = $(this).attr("id-for-selection")
    let value = $(this).attr("multi-value")
    
    Shiny.setInputValue(shinyInputId, value, {priority: "event"});
})
