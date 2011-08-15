package sia.classifier

import weka.classifiers.{Evaluation, Classifier}
import sia.Values._
import java.util.Random
import weka.core._
import weka.classifiers.trees.J48

object LPClassifier {
   private val letters = "ABCDEFGHJKLNPRSTUVWXYZ0123456789"

   private val letterVector = new FastVector {
      letters.foreach(c => addElement(c.toString))
   }

   private val boolVector = new FastVector {
      addElement(true.toString)
      addElement(false.toString)
   }

   private val idAttr = new Attribute(identifier, letterVector)

   private val numAttr = new Attribute(numLabel, boolVector)

   private val attList = idAttr :: numAttr :: createAttributes

   private val attrs = new FastVector {
      attList.foreach(addElement(_))
   }

   def apply(data: Instances) = {
      val classifier = new J48
      classifier.buildClassifier(data)
      classifier
   }

   def createData(seq: Seq[Map[String, Any]]) = {
      val trainingData = new Instances("Letters", attrs, 0)
      trainingData.setClassIndex(0)
      seq.foreach(p => trainingData.add(createTrainInstance(p)))
      trainingData
   }

   def evalPrint(classifier: Classifier, data: Instances) {
      val eval = new Evaluation(data)
      eval.crossValidateModel(classifier, data, 10, new Random)
      println(eval.toSummaryString)
      println(eval.toMatrixString)
   }

   def classify(data: Instances, classifier: Classifier) = {
      val enum = data.enumerateInstances
      var instance: Instance = null
      var letters = ""
      while(enum.hasMoreElements) {
         instance = enum.nextElement.asInstanceOf[Instance]
         val letterKey = classifier.classifyInstance(instance)
         letters += letterVector.elementAt(letterKey.intValue)
      }
      letters
   }

   private def createTrainInstance(attVals: Map[String, Any]) = {
      val instance = new Instance(attList.size)
      attList.foreach(a => attVals.getOrElse(a.name, 0) match {
         case s: String => instance.setValue(a, s)
         case d: Double => instance.setValue(a, d)
         case _ =>
      })
      instance
   }

   private def createAttributes = {
      val huAttrs = (1 to 7).map(i => new Attribute("Hu" + i)).toList

      val otherAttrs = attKeys.map(new Attribute(_))

      huAttrs ::: otherAttrs
   }
}