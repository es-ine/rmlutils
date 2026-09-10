![](RMLutils_hex_logo_v3_med.png)

# RMLutils

[![en](https://img.shields.io/badge/lang-en-red.svg)](README.md)
[![es](https://img.shields.io/badge/lang-es-yellow.svg)](README.es.md)

Librería de utilidades para machine learning en estadística oficial, que ofrece una interfaz común para múltiples librerías de aprendizaje automático.

La carpeta tutoriales contiene ejemplos de uso. Se recomienda revisar RMLUtils_info antes de progresar con el resto de tutoriales.

RMLUtils tiene una librería hermana implementada en Python con funcionalidades similares, llamada [pymlutils](https://github.com/es-ine/pymlutils).

## Trabajo en progreso

Este proyecto se encuentra en desarrollo.

## Descargo de responsabilidad

Este repositorio y su contenido se proporcionan únicamente con fines informativos y técnicos.  
El uso de este código es bajo su propia responsabilidad.

El Instituto Nacional de Estadística (INE) proporciona este software **"tal cual"**, sin garantía de ningún tipo, ya sea expresa o implícita.

Al utilizar este repositorio, usted reconoce que:
- Es responsable de revisar y probar el código antes de utilizarlo en cualquier entorno de producción o crítico.
- El Instituto Nacional de Estadística **no se hace responsable** de ningún daño directo, indirecto o consecuente que se derive del uso de este software.

## Índice
- [Características](#características)
- [Instalación desde github](#instalación-desde-github)
- [Instalación manual](#instalación-manual)
    - [Compilación e instalación de la librería local](#compilación-e-instalación-de-la-librería-local)
- [Autores](#autores)

## Características

1) Objetos sampler:
    * srsSampler
    * ssSampler
    * bootstrapSampler
    * cvSampler
    * cvGroupSampler
    * cvMixedSampler
    * cvTemporalSampler
    * fullSampler
2) Objetos modeler para clasificación:
    * H2OClassifierModeler
    * LGBClassifierModeler
    * rangerClassifierModeler
    * catboostClassifierModeler
    * miceClassifierModeler
    * preproClassifierModeler
3) Objetos modeler para regresión:
    * directRegressorModeler
    * HTRegressorModeler
    * strataHTRegressorModeler
    * strataMeanRegressorModeler
    * H2ORegressorModeler
    * LGBRegressorModeler
    * rangerRegressorModeler
    * catboostRegressorModeler
    * miceRegressorModeler
    * classifRegRegressorModeler
    * benchRegressorModeler
    * preproRegressorModeler
    * prePredRegressorModeler
    * memoryRegressorModeler
4) Funciones de coste para clasificación:
    * confusionMatrixCostFunction
    * accuracyCostFunction
    * precisionCostFunction
    * recallCostFunction
    * f1ScoreCostFunction
    * ROCAUCCostFunction
5) Funciones de coste para regresión:
    * r2CostFunction
    * squareBiasCostFunction
    * MSECostFunction
    * groupMSECostFunction
    * groupTotalErrorCostFunction
    * MCSubRBMSECostFunction
    * holdoutTemporalCostFunction
6) Funciones y objeto de preprocesado implementados:
    * preproObject
    * minMaxScalerPreproFunction
7) Mapeos de *score* a clases:
    * maxScoreClass
    * binaryThresholdClass
8) Objetos estimadores:
    * HTEstimate
    * HTDomainEstimate
    * REstimate
    * RDomainEstimate
    * MCSubRBEstimate
    * MCSubRBDomainEstimate
    * preTrainedEstimate
    * preTrainedDomainEstimate

Todos los modelers tienen múltiples métodos de utilidad:
- train
- get_predictions
- get_classes (solo para modelers clasificadores)
- cross_validate
- evaluate
- model_save
- model_load

Tenga en cuenta que los modelers de H2O y PreproModeler requieren de *inputs* adicionales por parte del usuario. Se recomienda consultar los markdowns de ejemplos que se encuentran en la carpeta "tutoriales".

Actualmente, este proyecto contiene modelos de ML de las siguientes librerías:

- [![Catboost](https://img.shields.io/badge/Catboost-f74931.svg)](https://catboost.ai/)
- [![LightGBM](https://img.shields.io/badge/LightGBM-8ef246.svg)](https://lightgbm.readthedocs.io/en/latest/R/index.html)
- [![H2O](https://img.shields.io/badge/H2O-fbfc79.svg)](https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/reference/h2o-package.html) 
- [![ranger](https://img.shields.io/badge/ranger-3a88fc.svg)](https://github.com/imbs-hl/ranger)
- [![mice](https://img.shields.io/badge/mice-9534eb.svg)](https://github.com/amices/mice)

## Requerimientos

**R 4.4.2 o superior**

Para mantener la instalación lo más ligera posible, RMLutils tan solo importa [data.table](https://github.com/rdatatable/data.table) y [zip](https://github.com/cran/zip). El usuario puede instalar los paquetes que necesite según los modeler que vaya a utilizar.

Los siguientes paquetes están disponibles en CRAN (de modo que pueden instalarse con el comando ``` install.packages('<package-name>')```):

- lightgbm   >=4.6.0     (usado en LGBModelers)
- h2o        >=3.44.0.3  (usado en H2OModelers)
- ranger     >=0.17.0    (usado en rangerModelers)
- mice       >=3.18.0    (usado en miceModelers)
- osqp       >=1.0.0     (usado en benchModeler)
- gsubfn     >=0.7       (usado en benchModeler)
- Matrix     >=1.7-1     (usado en benchModeler)

Algunos modelers de RMLutils también utilizan catboost, que puede instalarse siguiendo las instrucciones en este [enlace](https://catboost.ai/docs/en/concepts/r-installation). Si el usuario tuviera problemas instalando el paquete, se recomienda descargar el archivo tar.gz, descomprimirlo e instalarlo manualmente mediante el comando:

```
install.packages("/ruta/a/carpeta/descomprimida", 
repos = NULL, 
type = "source",
INSTALL_opts = c("--no-multiarch"))
```

Algunas versiones recientes pueden no funcionar. En dicho caso, recomendamos descargar una versión más antigua y comprobar que existe la carpeta DESCRIPTION en la carpeta descomprimida. Nuestro testeo se realizó mayormente en la versión 1.2.8 de catboost para windows.

## Estructura del proyecto

Los usuarios pueden utilizar RMLutils instalándolo manualmente o bien a través de Gitlab/Github.

Si el usuario quiere aprender o consultar cómo utilizar RMLutils, hay una carpeta con markdowns de ejemplo y tutoriales en la carpeta tutoriales, que detallan las características principales de RMLutils.

Se recomienda al usuario que se familiarice primero con los objetos sampler y modeler, antes de explorar el resto de elementos de la librería.

```text
root/
├── README.md
├── README.es.md
├── DESCRIPTION
├── NAMESPACE
├── man/
│   ├── accuracyCostFunction.Rd
│   ├── addMemory.Rd
│   └── ...
├── tutorials/
│   ├── es/
│   │   ├── sampler_tutorial.rmd
│   │   ├── modeler_tutorial.rmd
│   │   ├── cost_function_tutorial.rmd
│   │   ├── h2o_modeler_tutorial.rmd
│   │   ├── classif_reg_modeler_tutorial.rmd
│   │   ├── prepro_modeler_tutorial.rmd
│   │   └── ...
│   └── en/
│       └── ...
├── tests/
│   └── testthat
│       ├── test-classifierModeler.R
│       ├── test-classMapping.R
│       ├── test-costFunctions.R
│       ├── test-estimator.R
│       ├── test-regressorModeler.R
│       └── test-sampler.R
│
└── R/
    ├── classifierModeler.R
    ├── classMapping.R
    └── ...
```

## Instalación desde GitHub

Si el usuario desea instalar el paquete desde Github, debe comprobar primero que tiene instalada la librería remotes:

```bash
install.packages("remotes")
```

Si ya está instalada, debe ejecutar el siguiente comando:

```bash
remotes::install_github("es-ine/rmlutils")
```


## Instalación manual

Asumiendo que el usuario ha clonado el repositorio y que ha activado el proyecto de R, es posible instalar el paquete a través de la librería ```devtools```, ejecutando el siguiente comando:

```bash
devtools::build()
```

O, si el usuario utiliza RStudio, la librería puede compilarse haciendo click en el botón *Install* en la pestaña *Build* del editor.

### Compilación e instalación de la librería local

Cuando se hagan cambio sobre los scripts fuente de RMLutils, la librería puede re-compilarse con los comandos mencionados previamente. También es recomendable ejecutar los tests para comprobar que todos los componentes de la librería se comportan como sería esperable. Del mismo modo, si el usuario añade nuevas funcionalidades, debería añadir también tests unitarios.

Los tests pueden ejecutarse haciendo click en el botón *Test* en la pestaña *Build*, o con el siguiente comando:

```
devtools::test()
```

## Autores

* Diseñador principal: Luís Sanguiao Sande
* Programador principal: Jordi Verdú Naranjo
* Carlos Sáez Calvo
* Sandra Barragán Andrés
* María Novás Filgueira
* Juan Ródenas Gómez
* Beatriz Abad Martín
* Lucía Tello Nieto
* Juan Ramón Sesma Bernal
* Esther Puerto Sanz
* Miguel Anguita Ruiz
* Sergio Pardina
* Álvaro García
