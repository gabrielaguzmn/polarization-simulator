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