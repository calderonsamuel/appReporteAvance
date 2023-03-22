$(document).on('keyup', "input", function(event) {
    if ($(this).attr("type") === "text") {
      let $charCounter = $('#' + $(this).attr("id") + 'MlCounter');
      let currentLength = $(this).val().length;
      let maxLength = $(this).attr('maxlength')
      let ratio = currentLength / maxLength
      
      // Update the text of the small tag
      $charCounter.text(currentLength + ' / ' + maxLength);
      
      if (currentLength == maxLength) {
        $charCounter.removeClass("text-muted text-info").addClass("text-danger");
      } else if (ratio > 0.8) {
        $charCounter.removeClass("text-muted text-danger").addClass("text-info");
      } else {
        $charCounter.removeClass("text-info text-danger").addClass("text-muted");
      }
    }
  })