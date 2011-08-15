package sia.imagej

import org.scalatest.FunSuite

import sia.classifier.LPClassifier
import sia.Values._

class LoadTISuite extends FunSuite {
   test("Load training image") {
      val base = "src/main/resources/trainingimages/"
      val letterFeatures = IJLoader.loadDir(base)
      //println(letterFeatures.sortBy(_(identifier).toString).map(m => m(identifier) + ": " + m("Hu6") + "#" + m("Hu7")).mkString("\n"))
      val data = LPClassifier.createData(letterFeatures)
      val classifier = LPClassifier(data)

      //LPClassifier.evalPrint(classifier, data)

      val imgFeatures = IJLoader.loadImg("src/test/resources/golfgt_back.jpg")
      //println(imgFeatures.map(m => m("Hu6") + "#" + m("Hu7")).mkString("\n"))
      val imgData = LPClassifier.createData(imgFeatures)

      val letters = LPClassifier.classify(imgData, classifier)
      println("Result: " + letters)
   }
}