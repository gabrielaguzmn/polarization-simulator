import Benchmark._
import Opinion._
import Comete._

val frequencies1 = Vector(0.5, 0.5)
val distributionValues1 = Vector(0.0, 1.0)
val distribution1 = (frequencies1, distributionValues1)

val frequencies2 = Vector(0.05, 0.1, 0.15, 0.2, 0.25, 0.25)
val distributionValues2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
val distribution2 = (frequencies2, distributionValues2)

val frequencies3 = Vector(0.7, 0.2, 0.1)
val distributionValues3 = Vector(0.0, 0.5, 1.0)
val distribution3 = (frequencies3, distributionValues3)

val frequencies4 = Vector(0.2, 0.2, 0.2, 0.2, 0.2)
val distributionValues4 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val distribution4 = (frequencies4, distributionValues4)

val frequencies5 = Vector(0.1, 0.4, 0.4, 0.1)
val distributionValues5 = Vector(0.0, 0.3, 0.7, 1.0)
val distribution5 = (frequencies5, distributionValues5)

// Pruebas de la función `rhoCMT_Gen`
val cmt1 = rhoCMT_Gen (1.2, 1.2)
cmt1(distribution1)
cmt1(distribution2)
cmt1(distribution3)
cmt1(distribution4)
cmt1(distribution5)

// Pruebas de la funcion normalizar
val cmt1_normalizar = normalizar(cmt1)
cmt1_normalizar(distribution1)
cmt1_normalizar(distribution2)
cmt1_normalizar(distribution3)
cmt1_normalizar(distribution4)
cmt1_normalizar(distribution5)

val i1_30 = i1(30)
val i2_25 = i2(25)
val i1_50 = i1(50)
val i2_100 = i2(100)

//Pruebas de showWeightedGraph
showWeightedGraph(i1_30)
showWeightedGraph(i2_25)
showWeightedGraph(i1_50)
showWeightedGraph(i2_100)

val sb_ext= allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief (100)
val sb_midly = midlyBelief(100)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0 ,1.0)
val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)

rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)

rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)

rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief(100)
val sb_midly = midlyBelief(100)

val rhoPar1 = rhoPar(1.2,1.2)
val rhoPar2 = rhoPar(2.0,1.0)
val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rhoPar1(sb_ext, dist1)
rhoPar2(sb_ext, dist1)
rhoPar1(sb_ext, dist2)
rhoPar2(sb_ext, dist2)

rhoPar1(sb_cons, dist1)
rhoPar2(sb_cons, dist1)
rhoPar1(sb_cons, dist2)
rhoPar2(sb_cons, dist2)

rhoPar1(sb_unif, dist1)
rhoPar2(sb_unif, dist1)
rhoPar1(sb_unif, dist2)
rhoPar2(sb_unif, dist2)

rhoPar1(sb_triple, dist1)
rhoPar2(sb_triple, dist1)
rhoPar1(sb_triple, dist2)
rhoPar2(sb_triple, dist2)

rhoPar1(sb_midly, dist1)
rhoPar2(sb_midly, dist1)
rhoPar1(sb_midly, dist2)
rhoPar2(sb_midly, dist2)

// Pruebas funcion confBiasUpdate y confBiasUpdatePar

// Caso 1: Prueba con creencias uniformes y grafo i1
val numAgents1 = 10
val beliefs1 = uniformBelief(numAgents1)
val graph1 = i1(numAgents1)
val updatedBeliefs1 = confBiasUpdate(beliefs1, graph1)
val updatedBeliefs1Par = confBiasUpdatePar(beliefs1, graph1)

// Caso 2: Prueba con creencias altamente polarizadas y grafo i2
val numAgents2 = 20
val beliefs2 = allExtremeBelief(numAgents2)
val graph2 = i2(numAgents2)
val updatedBeliefs2 = confBiasUpdate(beliefs2, graph2)
val updatedBeliefs2Par = confBiasUpdatePar(beliefs2, graph2)

// Caso 3: Prueba con tres polos de creencias y grafo i1
val numAgents3 = 15
val beliefs3 = allTripleBelief(numAgents3)
val graph3 = i1(numAgents3)
val updatedBeliefs3 = confBiasUpdate(beliefs3, graph3)
val updatedBeliefs3Par = confBiasUpdatePar(beliefs3, graph3)

// Caso 4: Prueba con creencias de consenso y grafo i2
val numAgents4 = 50
val consensusValue = 0.7
val beliefs4 = consensusBelief(consensusValue)(numAgents4)
val graph4 = i2(numAgents4)
val updatedBeliefs4 = confBiasUpdate(beliefs4, graph4)
val updatedBeliefs4Par = confBiasUpdatePar(beliefs4, graph4)

val i1_10 = i1(10)
val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)
confBiasUpdatePar(sbu_10, i1_10)
rhoPar1(sbu_10, dist1)
rhoPar1(confBiasUpdatePar(sbu_10, i1_10), dist1)

confBiasUpdatePar(sbm_10, i1_10)
rhoPar1(sbm_10, dist1)
rhoPar1(confBiasUpdatePar(sbm_10, i1_10), dist1)
val i2_32768 = i2(32768)
val sbms = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val polSec = rho(1.2,1.2)

val evolSec = for {
  i <- 0 until sbms.length
} yield {
  simEvolucion(
    Seq(sbms(i), sbes(i), sbts(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial " ++ i.toString ++ "-" ++ sbms(i).length.toString
  )
}
// Prueba 1: Simulación con 5 creencias (2, 4, 8, 16, 32 agentes)
val sbms_1 = for {
  n <- 2 until 6
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes_1 = for {
  n <- 2 until 6
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts_1 = for {
  n <- 2 until 6
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolSec_1 = for {
  i <- 0 until sbms_1.length
} yield {
  simEvolucion(
    Seq(sbms_1(i), sbes_1(i), sbts_1(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial_1_" ++ i.toString ++ "-" ++ sbms_1(i).length.toString
  )
}

// Prueba 2: Simulación con 10, 20, 40, 80, 160 agentes
val sbms_2 = for {
  n <- 3 until 8
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes_2 = for {
  n <- 3 until 8
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts_2 = for {
  n <- 3 until 8
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolSec_2 = for {
  i <- 0 until sbms_2.length
} yield {
  simEvolucion(
    Seq(sbms_2(i), sbes_2(i), sbts_2(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial_2_" ++ i.toString ++ "-" ++ sbms_2(i).length.toString
  )
}

// Prueba 3: Simulación con 100, 200, 400, 800, 1600 agentes
val sbms_3 = for {
  n <- 5 until 10
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes_3 = for {
  n <- 5 until 10
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts_3 = for {
  n <- 5 until 10
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolSec_3 = for {
  i <- 0 until sbms_3.length
} yield {
  simEvolucion(
    Seq(sbms_3(i), sbes_3(i), sbts_3(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial_3_" ++ i.toString ++ "-" ++ sbms_3(i).length.toString
  )
}

// Prueba 4: Simulación con 50, 100, 200, 400, 800 agentes
val sbms_4 = for {
  n <- 6 until 11
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes_4 = for {
  n <- 6 until 11
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts_4 = for {
  n <- 6 until 11
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolSec_4 = for {
  i <- 0 until sbms_4.length
} yield {
  simEvolucion(
    Seq(sbms_4(i), sbes_4(i), sbts_4(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial_4_" ++ i.toString ++ "-" ++ sbms_4(i).length.toString
  )
}

// Prueba 5: Simulación con 10, 20, 40, 80, 160, 320 agentes
val sbms_5 = for {
  n <- 4 until 10
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes_5 = for {
  n <- 4 until 10
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts_5 = for {
  n <- 4 until 10
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolSec_5 = for {
  i <- 0 until sbms_5.length
} yield {
  simEvolucion(
    Seq(sbms_5(i), sbes_5(i), sbts_5(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "simulacion_secuencial_5_" ++ i.toString ++ "-" ++ sbms_5(i).length.toString
  )
}

// Comparacion versiones secuenciales contra paralelas

val polSec = rho(1.2, 1.2)
val polPar = rhoPar(1.2, 1.2)

val i132768 = i1(32768)
val i232768 = i2(32768)

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val sbms = for {
  n <- 2 until 13
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

println(compararMedidasPol(sbms, likert5, polSec, polPar))

println(compararFuncionesAct(sbms, i232768, confBiasUpdate, confBiasUpdatePar))