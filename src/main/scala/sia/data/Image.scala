package sia.data

class Image(data: Array[Array[Int]]) {

   private def extractMoment(f: (Int, Int) => Double) = {
      val values = for {
         x <- 0 until data.length
         y <- 0 until data.head.length
      } yield {
         f(x, y) * data(x)(y)
      }
      values.sum
   }

   private def extractRawMoment(p: Int, q: Int) = {
      extractMoment((x, y) => math.pow(x, p) * math.pow(y, q))
   }

   private def extractCentralMoment(p: Int, q: Int, xMean: Double, yMean: Double, m00: Double) = {
      val mom = extractMoment((x, y) => math.pow(y - xMean, p) * math.pow(x - yMean, q))
      mom / math.pow(m00, ((p + q) / 2) + 1)
   }

   def extractHuMoments = {
      val m00 = extractRawMoment(0, 0)
      val m10 = extractRawMoment(1, 0)
      val m01 = extractRawMoment(0, 1)

      //println("m00: " + m00 + " m10: " + m10 + " m01: " + m01)

      val xMean = m10 / m00
      val yMean = m01 / m00

      //println("xMean: " + xMean + " yMean: " + yMean)

      val cm = extractCentralMoment(_: Int, _: Int, xMean, yMean, m00)

      // Hu1
      val n20 = cm(2, 0)
      val n02 = cm(0, 2)

      val hu1 = n20 + n02

      // Hu2
      val n11 = cm(1, 1)

      val hu2 = math.pow(n20 - n02, 2) + 4 * math.pow(n11, 2)

      // Hu3
      val n30 = cm(3, 0)
      val n03 = cm(0, 3)
      val n21 = cm(2, 1)
      val n12 = cm(1, 2)

      val hu3 = math.pow(n30 - 3 * n12, 2) + math.pow(n03 - 3 * n21, 2)

      // Hu4
      val hu4 = math.pow(n30 + n12, 2) + math.pow(n03 + n21, 2)

      // Hu5
      val hu5 = (n30 - 3 * n12) * (n30 + n12) * (math.pow(n30 + n12, 2) - 3 * math.pow(n21 + n03, 2)) +
                (n03 - 3 * n21) * (n03 + n21) * (math.pow(n03 + n21, 2) - 3 * math.pow(n30 + n12, 2))

      // Hu6
      val hu6 = (n20 - n02) * (math.pow(n30 + n12, 2) - math.pow(n21 + n03, 2)) + 4 * n11 * (n30 + n12) * (n03 + n21)

      // Hu7
      val hu7 = (3 * n21 - n03) * (n30 + n12) * (math.pow(n30 + n12, 2) - 3 * math.pow(n21 + n03, 2)) +
                (n30 - 3 * n12) * (n21 + n03) * (math.pow(n03 + n21, 2) - 3 * math.pow(n30 + n12, 2))

      Map("Hu1" -> hu1, "Hu2" -> hu2, "Hu3" -> hu3, "Hu4" -> hu4, "Hu5" -> hu5, "Hu6" -> hu6, "Hu7" -> hu7)
   }
}