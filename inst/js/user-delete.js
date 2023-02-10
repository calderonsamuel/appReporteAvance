
$(function() {
    
    let classSelector = "user-delete"
    let shinyInputId = "isMac"
    
    $(document).on('shiny:connected', function(event) {
        
        let btns = document.getElementsByClassName(classSelector)
        let getNS = (str) => {
            let strWithoutId = str.replace(/[a-f0-9]{32}$/, "")
            return strWithoutId.length > 0 ? strWithoutId : ""
        }
        
        for (const element of btns) {
            element.addEventListener('click', (event) => {
                let idForSelection = element.attributes.idForSelection.value
                let ns = getNS(element.id)
                
                Shiny.setInputValue(ns + shinyInputId, idForSelection);
            });
        };
    });
});


/*
function custonShinyInputWrapper(classSelector = "user-delete", shinyInputId = "isMac") {
    
    function() {
        let classSelector = classSelector
        let shinyInputId = shinyInputId
        
        $(document).on('shiny:connected', function(event) {
            
            let btns = document.getElementsByClassName(classSelector)
            let getNS = (str) => {
                let strWithoutId = str.replace(/[a-f0-9]{32}$/, "")
                return strWithoutId.length > 0 ? strWithoutId : ""
            }
            
            for (const element of btns) {
                element.addEventListener('click', (event) => {
                    let idForSelection = element.attributes.idForSelection.value
                    let ns = getNS(element.id)
                    
                    Shiny.setInputValue(ns + shinyInputId, idForSelection);
                });
            };
        });
    }
}

$(custonShinyInputWrapper(classSelector = "user-delete", shinyInputId = "isMac"))
*/
