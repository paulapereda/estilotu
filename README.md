# estilotu
Paquete experimental con la marca de Transforma Uruguay para crear gráficos y shinydashboards con el estilo de la marca.

## Prerrequisitos

```
devtools::install_github('yixuan/sysfonts')
devtools::install_github('cttobin/ggthemr')
devtools::install_github('nik01010/dashboardthemes')
```

## Instalación

```
devtools::install_github('transformauy/estilotu')
library(estilotu)
```

## Gráficos

Para que los gráficos creados con ggplot utilicen por defecto el 'estilotu', luego de cargada la librería, se debe utilizar la siguiente función al principio del script:

```
set_estilotu()

g <- ggplot(mtcars, aes(mpg, cyl)) + geom_point()
g

```

## shinydashboard

Para utilizar el estilotu en los shinydashboards se debe:

 - tener siempre en la carpeta del proyecto los siguientes archivos que se encuentran en el [repositorio](https://github.com/paupereda/estilotu/tree/master/www): 'MyriadPro-Regular.otf' y 'estilo.css'
 - poner siempre en el 'dashboardSidebar' el siguiente código: **tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"))** (código CSS para customizar la fuente del shinydashboard) 
 - poner siempre en el 'dashboardBody': **estilo_tu** (función del paquete para customizar la apariencia del shinydashboard) 

## Colores

El paquete carga 7 colores de la marca. El nombre de los colores es: 'VERDE', 'NARANJA', 'GRIS', 'AZUL', 'AMARILLO', 'VERDE2' Y 'ROJO'. Es recomendable seguir el manual de marca para la elaboración de visualizaciones que empleen la variable 'Área de Política' del siguiente modo:

- Ciencia, tecnología e innovación: NARANJA
- Clima de negocios: GRIS
- Generación de capacidades - Humanas: VERDE2
- Generación de capacidades - Empresariales: VERDE
- Internacionalización: AZUL
- Hoja de Ruta: ROJO
