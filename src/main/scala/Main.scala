import scala.meta._

object Main extends App{
  val code = """val a: List[String]= List()""".parse[Stat]
  val caseExpr = """case true => println("its true!")""".parse[Case]
  val term = """x + y""".parse[Term]
  val arg = """a: List[String]""".parse[Term.Arg]

  printResult[Stat](code)
  printResult[Case](caseExpr)
  printResult[Term](term)
  printResult[Term.Arg](arg)

  def printResult[T](code: Parsed[T]): Unit = {
    code match {
      case Parsed.Success(tree) => println("Code is valid!")
      case Parsed.Error(pos, msg, details)  =>
        println(s"Pos: $pos, msg: $msg. More details: $details")
    }
  }
}
