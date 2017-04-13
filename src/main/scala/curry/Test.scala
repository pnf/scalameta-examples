package curry

/**
  * Created by pnf on 4/13/2017.
  */
class Bar {
  def apply(i: Int): Int => Int
}

object Test extends App {
  // @autoCurryMethods
  object Foo {
    def foo(i: Int, j: Int, k: Int): Int = i + j + k
    def foo(i: Int, j: Int)(k: Int) = i + j + k
    def foo(i: Int) = new Bar { def apply(j: Int) = (k: Int) => i + j + k}
  }

//  println(Foo.foo(1)(2)(3))
//  println(Foo.foo(1,2)(3))
  println(Foo.foo(1,2,3)) // Function3[Int x 4]   ... Function3[,,,]
  println(Foo.foo(1,2)(3))  // Function2[Int,Int,Function1[Int,Int]]  Function2[,,]
}

