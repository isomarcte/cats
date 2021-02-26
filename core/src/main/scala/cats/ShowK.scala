package cats

import scala.util.Try
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * ShowK is a universal show which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] can be shown.
 *
 * A ShowK[F] can produce a Show[F[A]] for any type A which has a Show[A]
 * instance.
 *
 * A common use case for this type is as a constraint for a GADT. For
 * example, consider the definition of some data type Foo[F] where the
 * primary F[_] values we are interested in are Option and Id.
 *
 * {{{
 * final case class Foo[F[_]](bar: F[Int], baz: F[String])
 * }}}
 *
 * If one wants to provide a Show instance for Foo, one needs to be able to
 * express a Show instance for F[Int] as well as F[String] for any F. Using
 * only Show (and not ShowK) this can be done as so,
 *
 * {{{
 * object Foo {
 *
 *   implicit def show[F[_]](implicit A: Show[F[Int]], B: Show[F[String]]): Show[Foo[F]] =
 *     new Show[Foo[F]] {
 *       override def show(t: Foo[F]): String =
 *         show"Foo(bar = ${t.bar}, baz = ${t.baz})"
 *     }
 * }
 * }}}
 *
 * The important requirement is that you will need to express a Show instance
 * for each distinct type in the given data type. This is perfectly
 * acceptable for data types which have a relatively small number of distinct
 * types, but as that number grows it becomes cumbersome to manage so many
 * constraints all of which are effectively them same underlying constraint
 * when viewed as a ShowK.
 *
 * Using ShowK our example becomes more simple.
 *
 * {{{
 * object Foo {
 *
 *   implicit def show[F[_]](implicit F: ShowK[F]): Show[Foo[F]] = {
 *     new Show[Foo[F]] {
 *       override def show(t: Foo[F]): String =
 *         showK"Foo(bar = ${t.bar}, baz = ${t.baz})"
 *     }
 *   }
 * }
 * }}}
 *
 * ShowK also provides a new StringContext similar to Show. For example,
 *
 * {{{
 * scala> showK"${Option(1)}"
 * val res0: String = Some(1)
 * }}}
 *
 * The ShowK StringContext also supports mixing types which only have a Show
 * instance, for example, the uses the Show instance for Int and the ShowK
 * instance for Option.
 *
 * {{{
 * scala> showK"${1} ${Option(1)}"
 * val res0: String = 1 Some(1)
 * }}}
 */
@implicitNotFound("Could not find an instance of ShowK for ${F}")
@typeclass trait ShowK[F[_]] extends Serializable {

  def showK[A](fa: F[A])(implicit A: Show[A]): String

  def algebra[A: Show]: Show[F[A]] =
    new Show[F[A]] {
      override def show(a: F[A]): String =
        showK(a)
    }
}

object ShowK {

  /**
   * A ShowK instance which always yields the same String regardless of the
   * value.
   */
  def const[F[_]](value: String): ShowK[F] =
    new ShowK[F] {
      override def showK[A](fa: F[A])(implicit A: Show[A]): String =
        value
    }

  final case class ShownK(override val toString: String) extends AnyVal

  sealed private[ShowK] trait ShownKLowPriority0 {
    implicit final def matShow[A](x: A)(implicit z: Show[A]): ShownK = ShownK(z.show(x))
  }

  object ShownK extends ShownKLowPriority0 {
    implicit def mat[F[_], A](x: F[A])(implicit y: ShowK[F], z: Show[A]): ShownK =
      ShownK(y.showK(x))
  }

  final case class ShowKInterpolator(_sc: StringContext) extends AnyVal {
    def showK(args: ShownK*): String = _sc.s(args: _*)
  }

  implicit def catsShowKForId: ShowK[Id] =
    fromComonad[Id]((fa: Id[String]) => fa)
  implicit def catsShowKForOption: ShowK[Option] =
    cats.instances.option.catsStdShowKForOption
  implicit def catsShowKForTry: ShowK[Try] =
    cats.instances.try_.catsStdShowKForTry

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ShowK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ShowK[F]): ShowK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllShowKOps[F[_], A](target: F[A])(implicit tc: ShowK[F]): AllOps[F, A] {
      type TypeClassType = ShowK[F]
    } = new AllOps[F, A] {
      type TypeClassType = ShowK[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ShowK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def showK(implicit A: Show[A]): String = typeClassInstance.showK[A](self)(A)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToShowKOps extends Serializable {
    implicit def toShowKOps[F[_], A](target: F[A])(implicit tc: ShowK[F]): Ops[F, A] {
      type TypeClassType = ShowK[F]
    } = new Ops[F, A] {
      type TypeClassType = ShowK[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToShowKOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */
}
