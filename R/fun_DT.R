options_DT <- function() {
  list(
    dom = "t",
    scrollX = TRUE,
    pageLength = 10,
    lengthMenu = c(10, 25, 40),
    language = list(
      emptyTable = "Cliente no tiene registros en esta sección",
      zeroRecords = "Cliente no existe. Buscar con otros datos o Agregar cliente",
      infoEmpty = "Datos no coinciden con ningún registro.",
      infoFiltered = "(filtrado de un total de _MAX_ registros)",
      lengthMenu = "Mostrar _MENU_ registros",
      info = "Mostrando _START_ al _END_ de _TOTAL_ registros",
      search = "Buscar:",
      paginate = list(
        previous = "Anterior",
        'next' = "Siguiente")
    )
  )
}
