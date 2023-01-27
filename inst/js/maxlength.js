// works when copied and pasted in browser console. needs a document.onload()

let textInput = document.getElementById("hi")
let counter = document.getElementById("hi-mlCounter")
// https://stackabuse.com/character-counter-for-text-areas-with-vanilla-javascript/

textInput.addEventListener('keydown', () => {
    counter.innerText = textInput.value.length + "/" + textInput.attributes.maxlength.value
})
