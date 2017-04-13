package curry

/**
  * Created by pnf on 4/13/2017.
  */
object Test extends App {
  @autoCurryMethods object Foo {
    def foo(i: Int, j: Int, k: Int) = i + j + k
  }

  println(Foo.foo(1)(2)(3))
}

