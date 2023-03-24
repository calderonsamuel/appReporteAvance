$(document).on('click', '.icon-picker', function(event) {
    let $element = $(this)
    let $svg_clicked = $element.children("svg")
    let $svg_to_replace = $element.parent().parent().siblings("button").children("svg")

    $svg_to_replace.replaceWith($svg_clicked.clone())
    console.log({
        svg: $svg_clicked,
        svg_to_replace: $svg_to_replace
    })
})