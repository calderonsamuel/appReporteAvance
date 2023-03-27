/**
 * Attaches a click listener to document that listens for clicks on elements with the class `.icon-picker`. 
 * When clicked, sets the selected icon inside the dropdown button label and applies the
 * text-primary style.
 * @function
 * @name iconPickerClickHandler
 * @param {Event} event - The click event object
 */
$(document).on('click', '.icon-picker', function(event) {
    let $element = $(this)
    
    // set the selected icon as label prefix in the dropdown button
    let $svg_clicked = $element.children("svg")
    let $svg_to_replace = $element.parent().parent().siblings("button").children("svg")
    
    $svg_to_replace.replaceWith($svg_clicked.clone())
    
    // remove the styling of the previously clicked icon
    let $last_clicked = $element.parent().siblings().find(".icon-last-clicked")
    $last_clicked.removeClass("text-primary icon-last-clicked").addClass("text-muted")
    
    // style the selected icon as bootstrap text-primary
    $element.removeClass("text-muted").addClass("text-primary icon-last-clicked");
})