import Comete._
import common._
import scala.collection. parallel.CollectionConverters._
package object Opinion {
  // Si n es el número de agentes, estos se identifican
  // con los enteros entre 0 y n−1
  // O sea el conjunto de Agentes A es
  // implicitamente el conjunto {0, 1, 2, ..., n−1}
  // Si b:BeliefConf, para cada i en Int, b[i] es un numero
  // entre 0 y 1 que indica cuanto creee la gente i en
  // la veracidad de la proposicion p
  // Si existe i: b(i) < 0 o b(i) > 1 b esta mal definida
  type SpecificBelief = Vector[Double]
  // Si b: SpecificBelief, para cada i en Int, b[i] es un
  // numero entre 0 y 1 que indica cuanto cree el
  // agente i en la veracidad de la proposicion p
  // El numero de agentes es b.length
  // Si existe i: b(i) < 0 o b(i) > 1 b esta mal definida.
  // Para i en Int\A, b(i) no tiene sentido
  type GenericBeliefConf = Int => SpecificBelief
  // si gb: GenericBelief, entonces gb(n) = b tal que
  // b: SpecificBelief
  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double

  // Si rho: AgentsPolMeasure y sb: SpecificBelief
  // y d: DistributionValues,
  // rho(sb, d) es la polarizacion de los agentes
  // de acuerdo a esa medida
  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete

  }

  // Tipos para Modelar la evolucion de la opinion en una red
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph =
    Int => SpecificWeightedGraph
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

  }

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    // Inicia un bucle externo para recorrer las filas de la matriz.
    // `swg._2` contiene el número de agentes, que define el tamaño de la matriz.
    for (i <- 0 until swg._2) yield {
      // Inicia un bucle interno para recorrer las columnas de la matriz.
      // Para cada combinación de fila `i` y columna `j`, calcula la influencia entre agentes.
      for (j <- 0 until swg._2) yield swg._1(i, j)
      // `swg._1` es la función de influencia, que toma los índices de dos agentes `(i, j)`
      // y devuelve el peso de la influencia de `i` sobre `j`.
    }
    // El resultado es una estructura `IndexedSeq` de `IndexedSeq`, donde cada fila representa
    // un agente y cada columna en esa fila representa su influencia sobre otros agentes.
  }
  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {

  }

  // Versiones paralelas
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete

  }

  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

  }
}