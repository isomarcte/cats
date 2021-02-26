package cats.tests

import cats._
import cats.syntax.show._
import cats.syntax.showK._

final class ShowKSuite extends CatsSuite {

  test("showK string interpolator") {
    case class Foo[F[_]](bar: F[Int], baz: F[String])

    object Foo {
      implicit def showInstance[F[_]: ShowK]: Show[Foo[F]] =
        new Show[Foo[F]] {
          override def show(t: Foo[F]): String =
            showK"Foo(bar = ${t.bar}, baz = ${t.baz})"
        }
    }

    assertEquals("Foo(bar = Some(1), baz = Some(baz))", Foo[Option](Some(1), Some("baz")).show)
  }

  test("showK string interpolator Show interpo") {
    assertEquals("1 Some(1)", showK"${1} ${Option(1)}")
  }
}
