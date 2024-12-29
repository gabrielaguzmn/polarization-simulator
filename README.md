# Simulador de Polarización en Redes

Este proyecto desarrolla un simulador de polarización utilizando principios de la programación funcional y concurrente en el lenguaje Scala. El simulador está diseñado para modelar y analizar cómo las opiniones de los agentes en una red social evolucionan con el tiempo bajo diversas influencias y condiciones.
El simulador integra métodos avanzados de análisis y modelos computacionales que permiten medir la polarización de las opiniones en la red. Estos métodos incluyen la capacidad de calcular métricas de polarización y observar cómo estas métricas cambian a lo largo del tiempo, proporcionando una visión detallada de la dinámica de la polarización.
Para mejorar el rendimiento y la eficiencia del simulador, se han implementado técnicas de paralelización. Estas técnicas permiten que los cálculos se realicen de manera concurrente, aprovechando al máximo los recursos del sistema y reduciendo significativamente el tiempo de ejecución. 
Esto es especialmente útil para simular redes grandes y complejas, donde los cálculos pueden ser intensivos y demorados.

## Estructura del proyecto

### Descripción del Paquete `Opinion`

El paquete `Opinion` contiene funciones y tipos para modelar y simular la evolución de opiniones en una red de agentes. A continuación se describen algunas de las funciones principales:

- `confBiasUpdate`: Actualiza la creencia específica basada en la influencia de un grafo ponderado específico.
- `confBiasUpdatePar`: Versión paralela de `confBiasUpdate` que utiliza paralelismo de tareas.
- `simulate`: Simula la evolución de las creencias en una red de agentes durante `t` pasos.
- `rho`: Calcula la medida de polarización de agentes basada en parámetros `alpha` y `beta`.
- `rhoPar`: Versión paralela de la medida de polarización basada en Comete.
- `showWeightedGraph`: Visualiza la matriz de influencias entre agentes.

### Descripción del Paquete `Comete`

El paquete `Comete` incluye las siguientes funciones:

- `min_p`: Encuentra el punto mínimo de una función convexa en un intervalo.
- `rhoCMT_Gen`: Calcula la medida de polarización comete para una distribución dada.
- `normalizar`: Normaliza los valores de polarización para que se encuentren en el rango [0,1].

## Instrucciones para Correr las Pruebas

Es indispensable que para algunas de las pruebas en el archivo se bajen los paquetes Benchmark y common.
Además, use una estructura similar a la siguiente para crear su archivo build.sbt:

`
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "Simulador de polarizacion en redes"
  )

scalacOptions ++= Seq("-deprecation")

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scalameta" %% "munit" % "0.7.26" % Test,
  "org.plotly-scala" %% "plotly-render" % "0.8.1"
)
`

Para ejecutar las pruebas definidas en el archivo `Pruebas.sc`, sigue estos pasos:

1. Asegúrate de tener instalado `sbt` (Scala Build Tool)
2. Abre una terminal en el directorio raíz del proyecto.
3. Ejecuta el siguiente comando para iniciar el interprete de Scala
`sbt console`
4. Copia los comandos en la terminal generada
