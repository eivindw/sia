package sia.imagej

import java.io.File

import ij.plugin.filter.Analyzer
import ij.{ImagePlus, IJ}
import ij.measure.ResultsTable
import ij.process.ImageProcessor

import sia.Values._
import sia.data.Image

object IJLoader {
   private val x = "X"
   private val areaKey = "Area"
   private val charMaxHeight = 70

   def loadDir(path: String) = {
      val dir = new File(path)
      val files = dir.listFiles.toSeq.filter(_.getName.endsWith(".jpg"))
      files.map(loadLP(_).getOrElse(Vector())).flatten
   }

   def loadFile(path: String) = {
      loadLP(new File(path)).get
   }

   def loadImg(path: String) = {
      val img = IJ.openImage(path)

      //img.getProcessor.setInterpolationMethod(ImageProcessor.BILINEAR)
      //img.getProcessor.rotate(-11.0)

      IJ.run(img, "Make Binary", null)

      img.setRoi(460, 504, 321, 63)
      val cropped = new ImagePlus("Cropped", img.getProcessor.crop)

      //cropped.show()

      thresholdAndConvert(cropped).zipWithIndex.map(p => p._1 + (numLabel -> (p._2 > 1).toString))
   }

   private def loadLP(file: File) = {
      val fileName = file.getName
      val letters = fileName.substring(0, fileName.indexOf("."))
      val img = IJ.openImage(file.getPath)
      val entries = thresholdAndConvert(img)

      if(letters.length != entries.size) {
         println("Warning: " + fileName + " expected " + letters.length + " tokens, found " + entries.size)
         None
      } else {
         Some(letters.map(_.toString).zip(entries).map(p => p._2 + (identifier -> p._1) + (numLabel -> p._1.matches("[0-9]").toString)))
      }
   }

   private def thresholdAndConvert(img: ImagePlus): Seq[Map[String, Any]] = {
      val resized = normalize(img)

      IJ.run(resized, "Make Binary", null)

      // area mean standard modal min centroid center perimeter bounding fit shape feret's integrated median skewness kurtosis area_fraction stack display redirect=None decimal=3
      IJ.run("Set Measurements...", "area centroid perimeter bounding fit shape feret's");
      IJ.run(resized, "Analyze Particles...", "size=500 circularity=0 show=Nothing clear include")

      //resized.show()

      val rt = Analyzer.getResultsTable
      if(rt.getCounter == 8) {
         removeSmallest(rt)
      }
      val table = convert(rt, List("BX", "BY", "Width", "Height", x) ::: attKeys: _*).sortBy(_(x))
      val charMatrix = toCharMatrix(table, resized)
      table.zip(charMatrix).map(z => z._1 ++ z._2.extractHuMoments)
   }

   private def toCharMatrix(table: Seq[Map[String, Double]], img: ImagePlus) = {
      for(entry <- table) yield {
         val bx = entry.getOrElse("BX", -1.0).toInt
         val by = entry.getOrElse("BY", -1.0).toInt
         val width = entry.getOrElse("Width", -1.0).toInt
         val height = entry.getOrElse("Height", -1.0).toInt

         new Image(
            (for(x <- bx until (bx + width)) yield {
               (for(y <- by until (by + height)) yield {
                  if(img.getPixel(x, y)(0) == 255) 1 else 0
               }).toArray
            }).toArray)
      }
   }

   private def normalize(img: ImagePlus) = {
      IJ.run(img, "Make Binary", null)

      IJ.run("Set Measurements...", "bounding");
      IJ.run(img, "Analyze Particles...", "size=50 circularity=0 show=Nothing clear")

      val rt = Analyzer.getResultsTable
      val maxHeight = rt.getColumn(rt.getColumnIndex("Height")).max
      val resizeFraction = charMaxHeight / maxHeight

      img.getProcessor.setInterpolationMethod(ImageProcessor.BILINEAR)
      new ImagePlus("Resized", img.getProcessor.resize((img.getWidth * resizeFraction).intValue))
   }

   private def convert(rt: ResultsTable, cols: String*) = {
      for(i <- 0 until rt.getCounter) yield {
         cols.map(s => (s, rt.getValue(s, i))).toMap
      }
   }

   private def removeSmallest(rt: ResultsTable) {
      var minArea = 10000.0
      var index = 0
      for(i <- 0 until rt.getCounter) {
         val area = rt.getValue(areaKey, i)
         if(area < minArea) {
            minArea = area
            index = i
         }
      }
      rt.deleteRow(index)
   }
}