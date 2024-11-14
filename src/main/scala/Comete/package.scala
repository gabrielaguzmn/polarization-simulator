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

  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {
    // Dados alpha y beta devuelve la funcion que calcula la medida
    // comete parametrizada en alpha y beta

    // Si Pi es una distribucion de opiniones y dv es un vector de
    // valores de distribucion, entonces rhoCMTGen(alpha, beta)(Pi, dv)
    // es la medida de polarizacion de los agentes de acuerdo a la
    // medida de polarizacion de comete parametrizada en alpha y beta
    // con distribucion Pi y valores de distribucion dv

  }

  def normalizar(m: PolMeasure): PolMeasure = {
    // Recibe una medida de polarizacion, y devuelve la
    // correspondiente medida que la calcula normalizada

  }
}