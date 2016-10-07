import java.io.FileWriter

import scala.meta._

object ConstantsValidator {
  case class Val(valName: scala.meta.Pat, valValue: String)

  def validate(source: Source): Any = source match {
    case source"..$stats" => stats.collect(_ match {
      case q"..$mods object ${Term.Name(name)} extends $template" if name == "Constants" => template match {
          case template"{ ..$stats2 } with ..$ctorcalls { $param => ..$stats3 }" =>{
            val vals: List[Val] = stats3.foldLeft(List[Val]()) {
              (acc, elem) => elem match {
                case q"..$mods2 val ..$patsnel: $tpeopt = $expr" => acc :+ Val(patsnel.head, expr.toString)
                case _ => acc
              }
            }
            vals.groupBy(_.valValue).foreach{ case
              (valueKey, listOfVals) => if (listOfVals.length > 1 ) throw new Exception(s"$valueKey is assigned more than once to different vals: ${listOfVals.map(_.valName)}")
            }
          }
        }
      }
    )
  }

  def validateName(source: Source) ={
    val fixedFile: Source = source match {
      case source"..$stats" => source"..${buildNewStatements(stats)}"
    }

    val fw = new FileWriter("src/main/scala/Constants.scala")
    fw.write(fixedFile.syntax)
    fw.close

  }

  private def buildNewStatements(stats: scala.collection.immutable.Seq[Stat]): List[Stat] = {
    stats.foldLeft(List[Stat]())((acc, elem) => elem match {
      case q"..$mods object ${Term.Name(name)} extends $template" =>
        val isFirstLetterOfObjectLowercase = Character.isLowerCase(name.head)
        if(isFirstLetterOfObjectLowercase){
          val newName = name.head.toString.toUpperCase + name.tail
          val objectWithFixedName = q"..$mods object ${Term.Name(newName)} extends $template"
          acc :+ objectWithFixedName
        }else {
          acc :+ q"..$mods object ${Term.Name(name)} extends $template"
        }
      case whatever => acc :+ whatever
    })
  }
}
