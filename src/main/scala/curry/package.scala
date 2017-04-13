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
              case x@q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" if paramss.size==1 && paramss.head.size>1 =>
                val paramsc = paramss.head.dropRight(1).map(List(_))
                val rp = paramss.last.last :: Nil
                val expr2 = q"(..$rp) => $expr"
                List(x,q"..$mods def $name[..$tparams](...$paramsc): $tpeopt = {$expr2}")
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

