# Modelización predictiva de la evolución térmica y su relación con indicadores ODS ambientales en Canarias

Repositorio asociado al Trabajo Final de Máster en Ciencia de Datos de la UOC del alumno Benjamín Rodríguez Díaz.

## Descripción

Este repositorio contiene el código y los productos derivados del análisis de la evolución térmica reciente de Canarias y su relación exploratoria con indicadores ambientales de los ODS 7, 13, 14 y 15.

## Estructura

- `TFM_canarias_pipeline_final.R`: script principal del pipeline.
- `.RData`: R Data file que contiene todo el environment ejecutado
- `data/`: datos brutos, procesados y metadatos.
- `outputs/tables/`: tablas generadas.
- `outputs/figures/`: figuras generadas.
- `outputs/models/`: modelos entrenados, si procede.
- `docs/`: documentación complementaria.

## Requisitos

- R >= 4.2
- Paquetes indicados en el script principal
- API key de AEMET configurada como variable de entorno (hay que pedirla a través de: https://opendata.aemet.es/centrodedescargas/obtencionAPIKey. Tiene una validez de 5 días):

```r
Sys.setenv(AEMET_API_KEY = "TU_API_KEY")
```

## Ejecución

Se deja tanto el script final .R como el .RData para poder cargar todo el environment sin tener que ejecutar todo el script.

```r
source("TFM_canarias_pipeline_final.R")
load(".RData")
```



