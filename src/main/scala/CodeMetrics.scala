import java.io.File

import model.Counts
import scala.meta._

object CodeMetrics {
  val allScalaFiles = recursiveListFiles(file("src/")).map(_.parse[Source]).collect{
    case Parsed.Success(tree) => tree}.toList

  val counts = allScalaFiles.foldLeft(Counts.initial)((acc, file) => {
    file match {
      case source"..$whateverItIsInFile" => whateverItIsInFile.foldLeft(acc)((accInFile: Counts, elem) => elem match {
        case q"..$mods object $name extends $template" =>
          accInFile.incObjectNo
        case q"..$mods class $tname[..$tparams] (...$paramss) extends $template" =>
          accInFile.incClassNo
        case q"..$mods trait $tname[..$tparams] extends $template" =>
          accInFile.incTraitNo
        case q"package object $name extends $template" =>
          accInFile.incPackageObjNo
        case _ => accInFile
      })
    }
  })

  private def file(filename: String) = new java.io.File(filename)
  private def recursiveListFiles(file: File): Iterable[File] = {
    if (file.exists()){
      val children = new Iterable[File] {
        def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
      }
      val newFile = if (file.isDirectory) Seq() else Seq(file)
      newFile ++: children.flatMap(recursiveListFiles)
    } else {
      new Iterable[File] {def iterator = Iterator.empty}
    }
  }
}
