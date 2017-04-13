/**
  * Created by pnf on 4/13/2017.
  */
import scala.meta._
package object curry {

  class autoCurryMethods extends scala.annotation.StaticAnnotation {
    inline def apply(defn: Any): Any = {
      meta {
        defn match {
          case q"..$mods object $name extends $template" =>
            val template"{ ..$statsEarly } with ..$ctorcalls { $param => ..$statsBody }" = template
            val statsBody2 = statsBody.flatMap { _ match {
              case orig@q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" if paramss.size==1 && paramss.head.size>1 =>
                val rp = paramss.last.last :: Nil
                val param"..$mods $paramname: $atpeopt = $expropt" = paramss.last.last
                println(s"..$mods $paramname: $atpeopt = $expropt")
                val ftype = for(lp <- atpeopt;
                    rp <- tpeopt;
                     lps = List(lp)) yield t"(..$lps) => $rp"
                val expr2 = q"(..$rp) => $expr"
                val params = paramss.head.dropRight(1)
                val curried = for(n <- 1 to params.size) yield {
                  val (p1,r) = params.splitAt(n) //   [1,2], [3]
                  val pcs = r.map(List(_)) // [[3][4]]
                  val paramss2 = p1 +: pcs  // (1,2)(3)(4)
                  q"..$mods def $name[..$tparams](...$paramss2): $ftype = {$expr2}"
                }
                val res = orig +: curried
                println(res)
                res
              case other => List(other)
            }}
            val template2 = template"{ ..$statsEarly } with ..$ctorcalls { $param => ..$statsBody2 }"
            q"..$mods object $name extends $template2"
          case _ =>
            throw new RuntimeException
            null
        }
      }
    }
  }
}

