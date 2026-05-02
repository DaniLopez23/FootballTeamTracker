# Football Team Tracker

Memoria breve del proyecto Football Team Tracker, una aplicación Shiny orientada al seguimiento del rendimiento de un equipo de fútbol a partir de partidos y eventos registrados.

## Descripción y objetivos

El objetivo principal del proyecto es centralizar la información deportiva de la temporada en un dashboard interactivo para analizar resultados, evolución competitiva y aportación individual de los jugadores. La aplicación permite consultar el estado general del equipo, revisar partidos concretos y comparar métricas de jugadores con una lectura rápida y visual.

## Estructura del dashboard y funcionalidades

El dashboard se organiza en cuatro secciones:

- **Equipo**: resume la temporada con KPIs de clasificación, puntos y diferencia de goles; incluye evolución por jornada, resultados, goles a favor y en contra, tarjetas y distribución de eventos por minuto.
- **Jugadores**: muestra los máximos referentes del equipo y rankings de goleadores, tarjetas y minutos jugados, además de la distribución de minutos por jugador.
- **Partidos**: lista los encuentros disponibles, permite seleccionar uno y visualizar el detalle del partido, la tabla de jugadores y la línea temporal de eventos.
- **Comparador**: ofrece una comparación paralela entre dos jugadores con métricas individuales y evolución por jornada.

Como filtros globales, la aplicación permite trabajar por localización, elegir jornadas concretas y acotar por primera o segunda vuelta. En varias gráficas se puede alternar entre valores por jornada y acumulados.

## Datos utilizados y origen

La app trabaja con un fichero Excel incluido en el proyecto: `inst/app/data/CAZALEGAS_B_DATA.xlsx`.

Las hojas principales usadas por el dashboard son:

- `Partidos`: base para la vista de equipo y el seguimiento de la clasificación, resultados y goles.
- `Eventos`: base para el análisis de jugadores, tarjetas, minutos y líneas temporales.
- `Equipos`: apoyo para resolver nombres de los rivales en la vista de partidos.

## Versión desplegada

La versión desplegada del dashboard está disponible en:

https://mymusapps.shinyapps.io/FootballTeamTracker/

## Fundamentos de visualización aplicados

El dashboard aplica una combinación de visualizaciones pensada para comparar, resumir y seguir la evolución temporal:

- **Series temporales** para observar la progresión por jornada en clasificación, goles, tarjetas y minutos.
- **Barras horizontales** para ordenar rankings de jugadores y facilitar la comparación entre muchos nombres.
- **Gráficos de tarta** para resumir proporciones de goles y tarjetas.
- **Timelines** para reconstruir el relato de cada partido minuto a minuto.
- **Filtros interactivos** para reducir la carga cognitiva y centrar el análisis en un tramo de temporada o en un contexto concreto.

La codificación de color se mantiene consistente: verdes para valores favorables, rojos para desfavorables, azules para información principal y amarillos para tarjetas o alertas. Esto refuerza la lectura rápida y la interpretación inmediata.

## Conclusiones y mejoras futuras

La aplicación convierte datos dispersos en una visión operativa de la temporada, útil para revisar rendimiento colectivo e individual con una interfaz compacta e interactiva. Como mejoras futuras, sería interesante añadir exportación de informes, nuevas comparativas entre jugadores o temporadas, mayor automatización en la carga de datos y visualizaciones complementarias para analizar zonas del campo o perfiles de rendimiento con más detalle.
