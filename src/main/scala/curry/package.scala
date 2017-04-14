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
              // Match on method definition with exactly one parameter group, containing more than one parameter
              case orig@q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" if paramss.size==1 && paramss.head.size>1 =>
                // Last parameter of first (and only group)
                val lp = paramss.head.last
                val lps = lp :: Nil
                // Destructure last parameter
                val param"..$mods $paramname: $atpeopt = $expropt" = lp
                // Construct type for function from last parameter type to method result type
                val ftype = for(lpt <- atpeopt;
                                rpt <- tpeopt;
                                lpts = List(lpt)) yield t"(..$lpts) => $rpt"
                // Construct lambda from last parameter to result
                val fexpr = q"(..$lps) => $expr"
                val params = paramss.head.dropRight(1)
                // Build list of curried forms.  E.g starting with f(i,j,k,l) = expr
                // construct
                //   f(i)(j)(k) = l => expr
                //   f(i,j)(k) = l => expr
                val curried = for(n <- 1 to params.size) yield {
                  val (p1,r) = params.splitAt(n) //   [1,2], [3]
                  val pcs = r.map(List(_)) // [[3][4]]
                  val paramss2 = p1 +: pcs  // (1,2)(3)(4)
                  val c = q"..$mods def $name[..$tparams](...$paramss2): $ftype = {$fexpr}"
                  // Uhm, how do we generate official compiler INFO?
                  println(s"Curried: $c")
                  c
                }
                // This is exciting, but for now we need to throw out all but fully curried form due to
                // type erasure issues.
                //val res = orig +: curried
                val res = orig :: curried.head :: Nil
                res
              case other => List(other)
            }}
            val template2 = template"{ ..$statsEarly } with ..$ctorcalls { $param => ..$statsBody2 }"
            q"..$mods object $name extends $template2"
          case _ =>
            // How do we generate ERROR?
            throw new RuntimeException
            null
        }
      }
    }
  }
}

