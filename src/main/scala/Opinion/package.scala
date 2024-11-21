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
    /**
     * @param specificBelief Vector de creencias (valores entre 0 y 1)
     * @param distributionValues Valores discretos para la distribución
     * @return Valor de polarización normalizado entre 0 (mín) y 1 (máx)
     */

    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      // Inicializamos el vector de frecuencias con ceros
      val emptyFrequencies = Vector.fill(distributionValues.length)(0.0)
      // Calculamos las frecuencias para cada valor de la distribución
      val rawFrequencies = specificBelief.foldLeft(emptyFrequencies) { (accumulatedFreq, agentBelief) =>
        // Encontramos el valor más cercano en la distribución para la creencia
        val closestValueIndex = distributionValues.indices.minBy { index =>
          math.abs(distributionValues(index) - agentBelief)
        }
        // Incrementamos la frecuencia para ese valor
        accumulatedFreq.updated(closestValueIndex, accumulatedFreq(closestValueIndex) + 1.0)
      }
      // Normalizamos las frecuencias dividiendo por el número total de agentes
      val totalAgents = specificBelief.length
      val normalizedFrequencies = rawFrequencies.map(frequency => frequency / totalAgents)
      // Creamos la distribución como un par de (frecuencias normalizadas, valores)
      val beliefDistribution = (normalizedFrequencies, distributionValues)
      // Creamos la medida de -comete- con los parámetros alpha y beta
      val baseCometeMeasure = Comete.rhoCMT_Gen(alpha, beta)
      // Normalizamos la medida respecto al peor caso(máxima polarización)
      val normalizedCometeMeasure = Comete.normalizar(baseCometeMeasure)
      // aqui se calcula y se devuelve la polarización normalizada
      normalizedCometeMeasure(beliefDistribution)
    }
  }


  // Tipos para Modelar la evolucion de la opinion en una red
  //type WeightedGraph = (Int, Int) => Double
  //type SpecificWeightedGraph = (WeightedGraph, Int)
  //type GenericWeightedGraph =
    //Int => SpecificWeightedGraph
    //type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

 //** def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

 // }

  //def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
  //}

  //def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {

 // }

  // Versiones paralelas
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    /**
     * Versión paralela de la medida de polarización basada en Comete.
     *
     * @param specificBelief Vector de creencias (valores entre 0 y 1)
     * @param distributionValues Valores discretos para la distribución
     * @return Valor de polarización normalizado entre 0 (mín) y 1 (máx)
     */
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      // Inicializamos el vector de frecuencias con ceros
      val emptyFrequencies = Vector.fill(distributionValues.length)(0.0)

      // Paralelizamos el cálculo de frecuencias usando ParVector
      val rawFrequencies = specificBelief.par.aggregate(emptyFrequencies)(
        // Función combinadora para cada creencia
        (accumulatedFreq, agentBelief) => {
          // Encontramos el valor más cercano en la distribución
          val closestValueIndex = distributionValues.indices.minBy { index =>
            math.abs(distributionValues(index) - agentBelief)
          }
          // Incrementamos la frecuencia para ese valor
          accumulatedFreq.updated(closestValueIndex, accumulatedFreq(closestValueIndex) + 1.0)
        },
        // Combina resultados parciales
        (freq1, freq2) => freq1.zip(freq2).map { case (a, b) => a + b }
      )

      // Normalizamos las frecuencias dividiendo por el número total de agentes
      val totalAgents = specificBelief.length
      val normalizedFrequencies = rawFrequencies.map(frequency => frequency / totalAgents)

      // Creamos la distribución como un par de (frecuencias normalizadas, valores)
      val beliefDistribution = (normalizedFrequencies, distributionValues)

      // Creamos la medida de Comete con los parámetros alpha y beta
      val baseCometeMeasure = Comete.rhoCMT_Gen(alpha, beta)

      // Normalizamos la medida respecto al peor caso (máxima polarización)
      val normalizedCometeMeasure = Comete.normalizar(baseCometeMeasure)

      // Calcula y devuelve la polarización normalizada
      normalizedCometeMeasure(beliefDistribution)
    }
  }

 // def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

  //}
}