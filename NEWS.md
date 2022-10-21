# appReporteAvance 0.2.5

# appReporteAvance 0.2.4

- Use fresh theme for user colors. fix #65

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




