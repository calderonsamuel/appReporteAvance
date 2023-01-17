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




