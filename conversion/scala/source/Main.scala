import java.io.File
import java.nio.file.{Files, Paths}
import scala.io.Source
import io.circe.generic.auto._
import io.circe.syntax._

case class DataRecord(id: String, name: String, category: String, value: String)

object CsvToJsonConverter {
  def main(args: Array[String]): Unit = {
    val inputPath = Paths.get("input")
    val outputPath = Paths.get("output")

    if (!Files.exists(outputPath)) {
      Files.createDirectories(outputPath)
    }

    val inputDir = inputPath.toFile
    if (inputDir.exists && inputDir.isDirectory) {
      val files = inputDir.listFiles().filter(_.getName.endsWith(".csv"))
      files.foreach(processFile(_, outputPath))
    }
  }

  def processFile(file: File, outputPath: java.nio.file.Path): Unit = {
    val source = Source.fromFile(file)
    val lines = source.getLines().toList
    source.close()

    if (lines.nonEmpty) {
      val records = lines.tail.map { line =>
        val cols = line.split(",").map(_.trim)
        DataRecord(cols(0), cols(1), cols(2), cols(3))
      }

      val jsonString = records.asJson.spaces2
      val outFileName = file.getName.replace(".csv", ".json")
      val destination = outputPath.resolve(outFileName)
      
      Files.write(destination, jsonString.getBytes("UTF-8"))
    }
  }
}
