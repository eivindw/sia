package sia.data

import org.scalatest.FunSuite

class ImageSuite extends FunSuite {
   test("Create image") {
      val img = new Image(Array(
         Array(0, 0, 1),
         Array(0, 1, 0),
         Array(1, 0, 1)))
      assert(img != null)

      println(img.extractHuMoments)
   }
}