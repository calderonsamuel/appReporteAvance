# appReporteAvance (development version)

# appReporteAvance 0.4.3

## Bug fixes

- Ya no existe discrepancia entre 1) la hora que el usuario escoge como plazo máximo en creación o edición de tarea, y 2) la hora que muestra el aplicativo una vez creada o editada una tarea.

## Otros

- Se incorpora un dockerfile para testeo local. Esto permite testear en un entorno muy similar al que se tiene en shinyapps.io.

# appReporteAvance 0.4.2

## Bug fixes

- Ahora es posible cambiar fecha limite de tareas. Fixes #134
- Vista de historial ya no muestra una fecha y hora incorrecta. Fixes #149
- Ahora se usa todo el espacio disponible en modal de edición de tarea. Fixes #148
- Ahora se usa todo el espacio disponible en el Modal de nuevo proceso. Fixes #146
- Ahora la vista de unidades de medida está dividida según corresponda a tarea y reportes. Fixes #147
- Ya no es obligatorio especificar metas de unidad de medida en gestión de tareas. Fixes #145

# appReporteAvance 0.4.1

## Bug fixes

- Arregla un bug que permitía que usuarios no responsables de equipo puedan archivar tareas terminadas y en revisión. Fixes #137
- Vista de historial ya no muestra mensaje de error. Fixes #133
- Se corrige bug que no permitía editar unidades de medida. Fixes #136
- Corrige un bug evitaba cargar los reportes al cambiar de grupo. Fixes #135

# appReporteAvance 0.4.0

## Procesos

La plataforma ahora incorpora una lógica de procesos. Debe entenderse que los equipos tienen procesos y los procesos tienen unidades de medida. Las unidades de medida pueden ser de tarea o de reporte. 

Esto implica que ahora al momento de añadir una tarea o reporte, debe seleccionarse un proceso para acceder a la lista de unidades de medida disponibles.

El panel de control se actualizó para incluir la gestión de procesos y sus unidades de medida. Para gestionar procesos se requiere ser responsable de equipo.

## Reportes

Los reportes son una nueva manera de informar acerca del progreso en las actividades. Mientras que las tareas suelen corresponder a actividades cotidianas o intermedias, los reportes deben entenderse como el resultado de un proceso. Como son de resultado, se añaden desde el panel de "Terminadas".

En un solo reporte se pueden informar acerca de progreso en múltiples unidades de medida.

## Estado 'Archivado' para tareas

Previamente, se mostraban todas las tareas no terminadas + las que se hayan terminado en los últimos 15 días. Esto significaba que implícitamente existía un estado 'Archivado' que no podía ser controlado por el usuario. 

Se decidió incorporar explícitamente el estado 'Archivado'. Ahora, la única manera de que no aparezcan las tareas terminadas (y reportes) es archivarlas manualmente.

## Abandono de {reportesAPI}

Se decide dejar de desarrollar el paquete `{reportesAPI}` de manera independiente porque dificultaba las tareas de desarrollo al tener que dividir la lógica de la administración de la data y su uso en la plataforma como proyectos independientes.

Esto no significa que la idea detrás de eso estuviera equivocada. Por el contrario, es fundamental que en el futuro se retome. Sin embargo, debe hacerse cuando realmente se intente administrar los datos desde una REST API, que no había sido el caso hasta ahora.

Todo el código desarrollado para ese paquete se ha migrado a este repositorio, donde se seguirá manteniendo administrando la base de datos desde clases `R6`.

## Otras pequeñas mejoras

- Ahora todos los campos de texto (por ejemplo al añadirle título a una tarea) muestran el límite máximo de caracteres.
- Se añadió un selector de íconos para las unidades de medida. Puede utilizarse al momento de añadir una nueva unidad de medida o al editarla.

## Bug fixes

- Se corrigió un error que no permitía obtener la vista de historial para tareas recién creadas.
- El campo de texto que contenía el nombre del miembro del equipo seleccionado para editarse daba la idea de que era posible cambiarle el nombre. Se corrigió para seguir viendo el nombre pero que quede claro que no se puede cambiar.


# appReporteAvance 0.3.8

- Remueve un botón que se mostraba y que no cumplía ninguna función.

# appReporteAvance 0.3.7

- Parche para asegurar consistencia con la nueva estructura de la base de datos, reportado en [`{reportesAPI}`](https://github.com/calderonsamuel/reportesAPI/releases/tag/v0.3.0)

# appReporteAvance 0.3.6

- Arreglado un bug que prevenía seguir reportando progreso de tareas después de haberle hecho un par de modificaciones (#18).
- Se incluye un panel de administración de grupo, que permite añadir integrantes al grupo, eliminarlos y editar sus roles y colores de tarjeta. Cada modificación se refleja en las tareas mostradas en el tablero.

# appReporteAvance 0.3.5

- La ventana de reporte de progreso cuenta con inputs más visibles
- La ventana de historial es más grande, logrando mostrar toda la tabla de historial

# appReporteAvance 0.3.4

- Se añadió un botón en el encabezado que permite acceder al manual
- Puede seleccionarse un grupo específico para ver las tareas correspondientes solo a ese grupo. Debe hacerse en el panel de configuración
- El calendario para añadir una nueva tarea tiene como fecha inicial el domingo

# appReporteAvance 0.3.3

## Card layout

La tarjeta de tarea tiene nueva distribución. Antes:

![image](https://user-images.githubusercontent.com/19418298/212998759-feb7a699-5888-4319-8ba3-2db27ec8a5fe.png)

Ahora:

![image](https://user-images.githubusercontent.com/19418298/212998535-3c43f9ab-1ef3-4904-8308-b546a5eee959.png)

- La fecha límite es mostrada como etiqueta
- El color de la etiqueta es relativo a la fecha límite (verde antes, amarillo durante, y rojo después)
- El avance y unidad de medida de meta se muestran en la parte inferior derecha
- Si el responsable de tarea y el asignador son la misma persona, el nombre aparece solo una vez

## Adición de tarea

- Ya no es necesario escoger organización y grupo para cada tarea nueva. La elección hecha en Configuración se aplica a las nuevas tareas. Esto significa que el tablero solo mostrará tareas de un solo grupo a la vez.

# appReporteAvance 0.3.2

- Se arregló un bug que mostraba "00" en lugar del número de mes en la interfaz de añadir tarea

# appReporteAvance 0.3.1

- Todos los usuarios ahora pueden explorar el historial de progreso de sus tareas (#62)
- Los responsables de equipo ahora pueden transferir su cargo a otro miembro del grupo (#90)
- Las tareas con modificaciones recientes aparecen primero en su respectivo listado (#91)
- Las tareas ahora tienen más opciones de unidad de resultado (#93)
- La clase R6 para el manejo de la base de datos se trabaja ahora aparte en [`{reportesAPI}`](https://github.com/calderonsamuel/reportesAPI)

# appReporteAvance 0.3.0

## UI 

- Las tareas ganan un nuevo estado posible: 'Observado' (#43)
- Las tarjetas de tarea ahora muestran la fecha límite (#63)
- Se incrementan los colores que pueden tener las tarjetas de tareas (#80)
- Administrador de grupo ahora puede ver y asignar tareas de otros miembros del equipo (#81, #82, #83, #86)

## Datos

- Se crearon clases R6 para el manejo de los datos. La clase base es un administrador
de bases de datos genérico sobre el cual se montaron clases de Organizaciones, Usuarios, Grupos y Tareas. (#78)

# appReporteAvance 0.2.5

- Los contenedores de las tarjetas de progreso se ajustan mejor a la pantalla y tienen scroll independiente.
- En el contenedor de tareas terminadas ahora solo se muestra como máximo las últimas 10 tareas terminadas. Esto mejora la experiencia para los usuarios responsables de equipo.


# appReporteAvance 0.2.4

- Se usa un tema 'fresh' para colores de usuarios asignados a tareas. fix #65

# appReporteAvance 0.2.3

- Ahora es posible reportar tareas Pendientes directamente a En revisión.
- El administrador puede editar información del registro de usuarios desde la interfaz de la aplicación

# appReporteAvance 0.2.2

- Se arregló un bug que impedía a user1 eliminar sus propias tareas.
- Se indica en la interfaz de reporte de progreso cuál es la tarea que se está modificando.
- Los usuarios ya no cuentan con privilegios globales sino empezaran a contar con roles en sus equipos.

# appReporteAvance 0.2.1

- Arreglado un bug que mostraba la pantalla correspondiente a usuarios no registrados junto con la ventana de inicio de sesión y reducía el tamaño de fuente de la app iniciada.

# appReporteAvance 0.2.0

- Ahora se utiliza una clase R6 para el cómputo de los datos necesarios para una sesión. En combinación con `reactiveValues()` ahora los datos de la sesión se comparten entre todos los módulos.
- Usuario responsable de equipo ya puede ver y modificar las tareas del resto del equipo. 
- Agregar plantilla, usuario, tarea y progreso ya no se renderiza desde el servidor.
- La app se inicia desde un `modalDialog()` para futura mayor facilidad en la personalización de la página de inicio.

# appReporteAvance 0.1.1

- mod_templates y mod_tasks ahora también disponibles para usuario tipo 'user1'
- Las tareas mostradas en mod_progress ahora tienen diferente color dependiente si el encargado de la tarea es un usuario o un grupo (Task: Box background diferente para grupos y usuarios #26)

# appReporteAvance 0.1.0

- Added a `NEWS.md` file to track changes to the package.
- First minimal version




