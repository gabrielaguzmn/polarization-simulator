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

  /**
   * @param specificBelief Vector de creencias
   * @param distributionValues Valores discretos para la distribución
   * @return Una función que toma una creencia específica (`SpecificBelief`) y valores de distribución
   *         (`DistributionValues`) y devuelve la polarización normalizada.
   */

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      // Número total de agentes en la red
      val totalAgents = specificBelief.length

      // Número de valores discretos en la distribución
      val k = distributionValues.length

      // Define los intervalos para clasificar las creencias:
      // Intervalo inicial: del inicio al primer punto medio
      val firstInterval = (0.0, distributionValues(1) / 2)

      // Intervalos intermedios: generados dinámicamente para los valores internos
      val middleIntervals = (1 to k - 2).map(i =>
        ((distributionValues(i) + distributionValues(i - 1)) / 2,
          (distributionValues(i) + distributionValues(i + 1)) / 2)
      ).toVector

      // Intervalo final: del último punto medio hasta 1.0
      val lastInterval = ((distributionValues(k - 2) + 1) / 2, 1.0)

      // Combina todos los intervalos
      val intervals = firstInterval +: middleIntervals :+ lastInterval

      // Inicializa un mapa vacío donde cada intervalo apunta a un vector vacío
      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap

      // Clasifica las creencias específicas dentro de los intervalos
      val classification = specificBelief.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if (i == k - 1) (start <= a && a <= end) // Último intervalo incluye el límite superior
          else (start <= a && a < end)            // Intervalos intermedios excluyen el límite superior
      })

      // Completa las clasificaciones faltantes y las ordena por clave
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      // Calcula las frecuencias normalizadas para cada intervalo
      val frequency = finalClassification.map { case (i, values) =>
        values.length.toDouble / totalAgents
      }.toVector

      // Obtiene la función auxiliar de medida de Comete con los parámetros alpha y beta
      val rhoAux = rhoCMT_Gen(alpha, beta)

      // Normaliza la medida para que esté en el rango [0, 1]
      val normalizarAux = normalizar(rhoAux)

      // Calcula y devuelve la polarización normalizada basada en las frecuencias y la distribución
      normalizarAux((frequency, distributionValues))
    }
  }

  // Tipos para Modelar la evolucion de la opinion en una red
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  type GenericWeightedGraph = Int => SpecificWeightedGraph
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  /**
   * Actualiza la creencia específica basada en la influencia de un grafo ponderado específico.
   *
   * @param sb  El vector de creencias específicas.
   * @param swg El grafo ponderado específico, representado como una tupla donde el primer elemento es una función
   *            que toma dos índices y devuelve el peso del borde entre ellos, y el segundo elemento
   *            es el número de agentes.
   * @return El vector de creencias específicas actualizado.
   */

  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    for {
      // Itera sobre cada creencia en el vector de creencias específicas
      belief <- sb

      // Encuentra los agentes influyentes, aquellos con un peso de borde mayor a 0
      influentAgents = (0 until swg._2).filter(j => swg._1(j, sb.indexOf(belief)) > 0)

      // Calcula la suma de las influencias de los agentes influyentes
      sum = (for {
        j <- influentAgents
        // Calcula el factor beta basado en la diferencia absoluta entre las creencias
        beta = 1 - math.abs(sb(j) - belief)
        // Obtiene el peso del grafo de influencia entre los agentes
        influenceGraph = swg._1(j, sb.indexOf(belief))
        // Calcula la influencia ajustada
        a = beta * influenceGraph * (sb(j) - belief)
      } yield a).sum

      // Calcula la nueva creencia basada en la suma de influencias
      nb = belief + sum / influentAgents.length
    } yield nb
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
    // Itera `t` pasos para actualizar las creencias y aplica la funcion de actualizacion
    (0 until t).scanLeft(b0)((currentBelief, _) => fu(currentBelief, swg))
  }

  // Versiones paralelas

  /**
   * Versión paralela de la medida de polarización basada en Comete.
   *
   * @param specificBelief     Vector de creencias (valores entre 0 y 1)
   * @param distributionValues Valores discretos para la distribución
   * @return Valor de polarización normalizado entre 0 (mín) y 1 (máx)
   * @param alpha Parámetro que controla la forma de la medida de polarización.
   * @param beta Parámetro que controla la sensibilidad de la medida a las diferencias.
   */

  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      val totalAgents = specificBelief.length

      // Número de valores discretos en la distribución
      val k = distributionValues.length

      // Define los intervalos inicial, intermedios y final de la distribución
      val firstInterval = (0.0, distributionValues(1) / 2)

      // Calcula los intervalos intermedios utilizando paralelización
      val middleIntervals = (1 to k - 2).par.map(i =>
        ((distributionValues(i) + distributionValues(i - 1)) / 2, (distributionValues(i) + distributionValues(i + 1)) / 2)).toVector
      val lastInterval = ((distributionValues(k - 2) + 1) / 2, 1.0)

      // Concatena los intervalos inicial, intermedios y final
      val intervals = firstInterval +: middleIntervals :+ lastInterval

      // Crea una clasificación inicial vacía donde cada intervalo apunta a un vector vacío
      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap

      // Clasifica las creencias específicas dentro de los intervalos correspondientes
      val classification = specificBelief.par.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if (i == k - 1) (start <= a && a <= end) // Intervalo final incluye el límite superior
          else (start <= a && a < end)  // Intervalos intermedios excluyen el límite superior
      })

      // Combina la clasificación vacía con la clasificación calculada y la ordena por clave
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      // Calcula las frecuencias normalizadas para cada intervalo
      val frequency = finalClassification.map { case (i, values) => values.knownSize.toDouble / totalAgents }.toVector

      // Obtiene la función auxiliar de medida de Comete con los parámetros alpha y beta
      val rhoAux = rhoCMT_Gen(alpha, beta)

      // Normaliza la medida para que esté en el rango [0, 1]
      val normalizarAux = normalizar(rhoAux)

      // Calcula y devuelve la polarización normalizada basada en las frecuencias y la distribución
      normalizarAux((frequency, distributionValues))
    }
  }

  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    sb.par.map { belief =>
      // Encuentra los agentes influyentes en paralelo
      val influentAgents = (0 until swg._2).par.filter(j => swg._1(j, sb.indexOf(belief)) > 0)

      // Calcula la suma de las influencias en paralelo
      val sum = influentAgents.map { j =>
        val beta = 1 - math.abs(sb(j) - belief) // Factor de ajuste
        val influenceGraph = swg._1(j, sb.indexOf(belief)) // Peso del grafo
        beta * influenceGraph * (sb(j) - belief) // Influencia ajustada
      }.sum

      // Calcula la nueva creencia basada en las influencias
      val nb = if (influentAgents.nonEmpty) belief + sum / influentAgents.size else belief
      nb
    }.seq
  }
}