# Football Team Tracker

Memoria breve del proyecto Football Team Tracker, una aplicación Shiny orientada al seguimiento del rendimiento de un equipo de fútbol a partir de los partidos de una temporada y eventos básicos registrados.

## Autor

Daniel López Paredes

## Descripción y objetivos

Football Team Tracker es una aplicación Shiny pensada para concentrar en un único entorno la información generada por un equipo de fútbol a lo largo de una temporada. A partir de datos de partidos y eventos básicos, la herramienta organiza la información para que el usuario pueda seguir la evolución del equipo, revisar encuentros concretos y entender el impacto de cada jugador en el rendimiento general. El enfoque del proyecto combina análisis descriptivo y visualización interactiva para transformar datos dispersos en una lectura clara, rápida y útil tanto a nivel colectivo como individual.

Objetivos del proyecto:

1.  Centralizar la información deportiva de la temporada en un dashboard interactivo que facilite la consulta de resultados, clasificaciones, eventos y métricas de jugadores.
2.  Analizar el rendimiento colectivo del equipo a través de la evolución por jornadas, los resultados de cada partido y la distribución de goles, tarjetas y minutos jugados.
3.  Realizar comparaciones básicas entre jugadores mediante visualizaciones sencillas que permitan contrastar su participación y sus métricas principales de forma rápida.

## Estructura del dashboard y funcionalidades

El dashboard se divide en 3 partes principales:

-   **Navegador superior**: permite acceder a las secciones de Equipo, Jugadores, Partidos y Comparador.

-   **Sidebar izquierdo**: contiene filtros globales para seleccionar localización (local/visitante), jornadas concretas y primera o segunda vuelta.

-   **Contenido central**: se adapta a cada sección mostrando visualizaciones específicas para el análisis de equipo, jugadores, partidos o comparaciones.

En cuanto a las cuatro secciones principales accesibles desde el navegador en la parte superior son:

-   **Equipo**: resume la temporada con KPIs de clasificación, puntos y diferencia de goles; incluye evolución por jornada, resultados, goles a favor y en contra, tarjetas y distribución de eventos por minuto.
-   **Jugadores**: muestra los máximos referentes del equipo y rankings de goleadores, tarjetas y minutos jugados, además de la distribución de minutos por jugador.
-   **Partidos**: lista los encuentros disponibles, permite seleccionar uno y visualizar el detalle del partido, la tabla de jugadores y la línea temporal de eventos. En esta sección el sidebar de filtros se sustituye por una lista con los partidos disponibles para seleccionar.
-   **Comparador**: ofrece una comparación paralela entre dos jugadores con métricas individuales y evolución por jornada.

De esta manera, la aplicación mantiene la misma estructura de navegación y filtros en todas las secciones, cambiando únicamente el contenido principal de cada sección. Esto facilita la experiencia de usuario y permite un análisis fluido tanto a nivel colectivo como individual.

## Datos utilizados y origen

Los datos han sido recolectados manualmente de la página web de la [Federación de Fútbol de Castilla-La Mancha](https://www.ffcm.es/pnfg/NPcd/NFG_VisGrupos_Vis?cod_primaria=1000123&codgrupo=22264089#google_vignette) , a partir de los partidos disputados por el equipo C.D Cazalegas Ébora Fromación en la temporada 2025-2026 hasta la Jornada 22. Se han registrado resultados, goles, tarjetas, minutos jugados y eventos relevantes para cada encuentro. Para los jugadores del C.D Cazalegas Ébora Formación se han registrado además los minutos jugados, titularidades y suplencias de cada jugador. Para los rivales si se identifica el equipo al que pertenecen pero no el nombre del jugador. La información se ha organizado en un formato estructurado para facilitar su uso en la aplicación Shiny.

Estos datos se han recopilado en un Excel el cual está incluido en el proyecto: `inst/app/data/CAZALEGAS_B_DATA.xlsx`.

Las hojas principales usadas por el dashboard son:

-   `Partidos`: base para la vista de equipo y el seguimiento de la clasificación por jornada, resultados y goles.
-   `Eventos`: base para el análisis de jugadores con sus goles, tarjetas, minutos y cambios.
-   `Equipos`: apoyo para resolver nombres de los equipos rivales en la vista de partidos.

Debido a motivaciones de privacidad, no se han incluido datos personales de los jugadores ni detalles de eventos que puedan identificar a jugadores de otros equipos. La aplicación se centra exclusivamente en el seguimiento del equipo C.D Cazalegas Ébora Formación y sus jugadores. Para más información contactar con el autor de la aplicación.

## Versión desplegada

La versión desplegada del dashboard está disponible en:

<https://mymusapps.shinyapps.io/FootballTeamTracker/>

El código fuente de la aplicación se encuentra en el repositorio de GitHub:

<https://github.com/DaniLopez23/FootballTeamTracker>

## Fundamentos de visualización aplicados

El dashboard aplica una combinación de visualizaciones pensada para comparar, resumir y seguir la evolución temporal:

-   **Series temporales** para observar la progresión por jornada en clasificación, goles, tarjetas y minutos.
-   **Barras horizontales** para ordenar rankings de jugadores y facilitar la comparación entre muchos nombres.
-   **Gráficos de tarta** para resumir proporciones de goles y tarjetas.
-   **Timelines de eventos** para reconstruir el relato de cada partido minuto a minuto.
-   **Filtros interactivos** para reducir la carga cognitiva y centrar el análisis en un tramo de temporada o en un contexto concreto.

La codificación de color se mantiene consistente: verdes para valores favorables, rojos para desfavorables, azules para información principal y amarillos para tarjetas o alertas. Esto refuerza la lectura rápida y la interpretación inmediata.

## Conclusiones y mejoras futuras

La aplicación convierte datos dispersos en una visión operativa de la temporada, útil para revisar rendimiento colectivo e individual con una interfaz compacta e interactiva. Como mejoras futuras, sería interesante añadir exportación de informes para extraer resúmenes de cada jornada. Por otro lado se podría establecer una mayor automatización en la carga de datos mediante web scrapping o permitiendo al usuario introducir los datos en la aplicación.