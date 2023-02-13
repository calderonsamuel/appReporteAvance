let getNS = (str) => {
    let strWithoutId = str.replace(/[a-f0-9]{32}$/, "")
    return strWithoutId.length > 0 ? strWithoutId : ""
}

function setMultiBtnShinyInput(shinyInputId, classSelector) {
    $(document).on('click', classSelector, function(event) {
        let idForSelection = $(this).attr("id-for-selection")
        let ns = getNS(this.id)
        
        Shiny.setInputValue(ns + shinyInputId, idForSelection);
    })
}

setMultiBtnShinyInput("userToDelete", ".user-delete")
setMultiBtnShinyInput("userToEdit", ".user-edit")
