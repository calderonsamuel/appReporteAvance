$(document).on('keyup', "textarea, input[type='text']", function(event) {
  let $charCounter = $(this).siblings("small");
  let currentLength = $(this).val().length;
  let maxLength = $(this).attr('maxlength')
  let ratio = currentLength / maxLength
  
  // update character counter text
  $charCounter.text(currentLength + ' / ' + maxLength);

  // update character counter text color
  if (ratio == 1) {
    $charCounter.removeClass("text-muted text-info").addClass("text-danger");
  } else if (ratio > 0.8) {
    $charCounter.removeClass("text-muted text-danger").addClass("text-info");
  } else {
    $charCounter.removeClass("text-info text-danger").addClass("text-muted");
  }
})