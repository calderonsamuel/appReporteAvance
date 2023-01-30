// works when copied and pasted in browser console. needs a document.onload()

function textInputCounter(elementId) {
    let textInput = document.getElementById(elementId)
    let counter = document.getElementById(elementId + "-mlCounter")
    // https://stackabuse.com/character-counter-for-text-areas-with-vanilla-javascript/
    
    textInput.addEventListener('keydown', () => {
        counter.innerText = textInput.value.length + "/" + textInput.attributes.maxlength.value
    })
}

console.log('hi external')

window.onload = (event) => {
    textInputCounter('hi')
    console.log('hi internal')
};

// textInputCounter('hi')

// document.onload = (event) => textInputCounter('hi')
