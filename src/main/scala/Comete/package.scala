package object Comete {
  type DistributionValues = Vector[Double]
  // Tipo para los valores, reales ordenados entre 0 y 1,
  // incluidos 0 y 1, de una distribucion
  type Frequency = Vector[Double]
  // Pi_k es una frecuencia de longitud k
  // si Pi_ k.lenght = k, 0 <= Pi_k(i) <= 1, 0 <=i <= k−1
  // y Pi_k.sum == 1
  type Distribution = (Frequency, DistributionValues)
  // (Pi, dv) es una distribucion si pi es una Frecuencia
  // y dv son los valor es de distribucion y pi y dv son
  // de la misma longitud

  type PolMeasure = Distribution => Double

  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    // Devuelve el punto p en [min, max] tal que f(p) es minimo
    // Suponiendo que f es convexa
    // si max−min < prec, devuelve el punto medio de [min, max]
    // Si el intervalo es menor que la precisión deseada, devuelve el punto medio como aproximación
    if (max - min < prec) (min + max) / 2
    else {
      // Divide el intervalo en 10 partes
      val intervalSize = (max - min) / 10
      val points = (0 to 10).map(i => min + i * intervalSize)

      // Encuentra el punto en el que f es mínimo
      val minPoint = points.minBy(f)
      val minIndex = points.indexOf(minPoint)

      // Define los nuevos límites del subintervalo que contiene el punto mínimo
      val newMin = if (minIndex == 0) min else points(minIndex - 1)
      val newMax = if (minIndex == 10) max else points(minIndex + 1)

      // Llama recursivamente a min_p en el nuevo intervalo
      min_p(f, newMin, newMax, prec)
    }
  }

  // Define `rhoCMT_Gen` que toma `alpha` y `beta` (ambos Double) y devuelve una función `PolMeasure`.
  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {

    // La función resultante recibe una `distribution` (una tupla de listas `pi` y `dv`).
    distribution =>
      val (pi, dv) = distribution

      // Función auxiliar `rho_aux` que calcula una métrica de error en base a `p`.
      def rho_aux(p: Double): Double = {
        // Combina `pi` y `dv`, aplica ponderación con `alpha` y `beta`, y suma los resultados.
        pi.zip(dv).map { case (prob, value) =>
          math.pow(prob, alpha) * math.pow(math.abs(value - p), beta)
        }.sum
      }

      // Encuentra el valor `p` en [0.0, 1.0] que minimiza `rho_aux` con precisión de 0.001.
      val optimalP = min_p(rho_aux, 0.0, 1.0, 0.001)

      // Calcula `rho_aux(optimalP)`, redondea a tres decimales y convierte a Double.
      BigDecimal(rho_aux(optimalP)).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def normalizar(m: PolMeasure): PolMeasure = {
    // Recibe una medida de polarizacion, y devuelve la
    // correspondiente medida que la calcula normalizada

  }
}