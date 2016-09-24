import scala.meta._

object Main extends App{
  val tokenCode =
    """
      defte st Method = {
        println("printing");
    """.tokenize

  val tokens = tokenCode match {
    case Tokenized.Success(t) => t
    case Tokenized.Error(_, _, details) => throw new Exception(details)
  }

  println(tokens.structure)
  println(tokens.syntax)

  val code =
    """case class Car[CarCompany](brand: CarCompany, color: Color, name: String){
         val owner: String = "John"
         def playRadio() = {
           "playing radio"
         }
         val capacity, speed = (5, 200)
         val oneVal = 45
      }
    """.parse[Stat]

  val q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" = parseCode(code)

  template match {
    case template"{ ..$stats } with ..$ctorcalls { $param => ..$stats2 }" => stats2.map{
      case q"..$mods def $name[..$tparams](...$paramss): $tpe = $expr" => println(s"methodName: $name")
      case q"..$mods val ..$patsnel: $tpeopt = $expr" => println(s"value $patsnel equals to $expr")
    }
  }

  val constructedTree = q"""def foo = println("quasiquotes")"""

//  println(constructedTree.show[Structure])

  def parseCode[T](code: Parsed[T]): T = {
    code match {
      case Parsed.Success(tree) => tree
      case Parsed.Error(pos, msg, details)  => throw new Exception(msg)
    }
  }
}