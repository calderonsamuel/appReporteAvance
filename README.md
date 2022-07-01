
<!-- README.md is generated from README.Rmd. Please edit that file -->

# appReporteAvance

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

El objetivo de `{{appReporteAvance}}` es contar con un aplicación web
donde los usuarios puedan reportar el avance de sus tareas. Cuenta con
módulos que pueden ser accedidos por usuarios según los privilegios de
su cargo. El diseño de la app busca ser suficientemente general como
para ser utilizado por cualquier tipo de organización, pero se dará
preferencia a los avances que ayuden a organizaciones gubernamentales.

## Importante

Este paquete aún se encuentra en desarrollo, por lo que es posible que
existan cambios sustanciales en su funcionamiento.

## Instalación

Puedes instalar la versión en desarrollo de `{{appReporteAvance}}` con
el siguiente código:

``` r
pak::pkg_install("calderonsamuel/appReporteAvance")
```

Alternativamente, puedes usar `{{devtools}}` o `{{remotes}}`.

``` r
remotes::install_github("calderonsamuel/appReporteAvance")
```

## Requerimientos

### Base de datos

Para implementar una app a partir del código proporcionado se necesita
conocer un mínimo sobre bases de datos estructuradas.

La app utiliza funciones que se conectan a un BD, pero esta debe
proveerla el mismo usuario. Para ello, debe especificar cómo conectarse
a la BD. Por ejemplo, para conectarse a una **MariaDB** se colocan las
siguientes environment variables en el archivo `.Renviron`:

-   `DB_HOST`
-   `DB_PORT`
-   `DB_NAME`
-   `DB_USER`
-   `DB_SECRET`

En el archivo `R/fun_db_general.R` se define una función para conectarse
a la DB.

``` r
db_connect <- function() {
  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"))
}
```

No olvides agregar `.Renviron` a tu archivo `.gitignore` y tomar medidas
de seguridad adecuadas.

### Firebase

Para evitar el acceso de personas sin autorización, se utiliza el
servicio **Firebase** de Google a través del paquete `{{firebase}}`.

Para una guía rápida de cómo implementar un servidor Firebase ver la
sección [“Get
Started”](https://firebase.john-coene.com/guide/get-started/) de la
documentación de `{{firebase}}`.

Se requiere indicar las siguientes environment variables en el archivo
`.Renviron`:

-   `FIREBASE_API_KEY`
-   `FIREBASE_PROJECT_ID`
-   `FIREBASE_AUTH_DOMAIN`
-   `FIREBASE_STORAGE_BUCKET`
-   `FIREBASE_APP_ID`

Para mayores detalles, ver la sección
[“Config”](https://firebase.john-coene.com/guide/config/) de la
documentación. Toma en cuenta que ahí se te indicará que, además de las
mencionadas, se requiere la variable `FIREBASE_DATABASE_URL`. Pero el
paquete en realidad nunca la busca, así que no es realmente necesaria.

Nuevamente, no olvides agregar `.Renviron` a tu archivo `.gitignore` y
tomar medidas de seguridad adecuadas.
