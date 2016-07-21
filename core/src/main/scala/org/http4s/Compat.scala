package org.http4s

import cats.data.Xor
import fs2.interop.cats._

trait Compat extends Instances {
  type Task[+A] = fs2.Task[A]
  val Task = fs2.Task

  type Process[+F[_], +O] = fs2.Stream[F, O]
  implicit class ProcessSyntax[F[_], O](self: Process[F, O]) {
    def runFoldMap[O2](f: O => O2): F[O2] =
      self.runFoldMap(f)

    def foldMonoid(implicit O: cats.kernel.Monoid[O]) =
      fs2.interop.cats.StreamCatsOps(self).foldMonoid

    def kill: Process[F, O] =
      self.open.close
  }
  def emit(a: A): fs2.Stream[Nothing, A] =
    fs2.Stream.emit(a)
  def eval[F[_], A](fa: F[A]): fs2.Stream[F, A] =
    fs2.Stream.eval(fa)
  def eval_[F[_], A](fa: F[A]): fs2.Stream[F, Nothing] =
    fs2.Stream.eval_(fa)
  def halt[A] =
    fs2.Stream.empty

  type Kleisli[F[_], A, B] = cats.data.Kleisli[F, A, B]
  val Kleisli = cats.data.Kleisli

  implicit class IdSyntax[A](self: A) {
    def left[B]: A Xor B =
      Xor.left(self)

    def right[A]: A Xor B =
      Xor.right(self)
  }

  type \/[+A, +B] = Xor[A, B]
  object \/ {
    def fromTryCatchNonFatal[A](f: => A): Throwable Xor A =
      Xor.catchNonFatal(f)
  }
  object -\/ {
    def apply[A, B](a: A): A Xor B =
      Xor.left(a)
  }
  object \/- {
    def apply[A, B](b: B): A Xor B =
      Xor.right(b)
  }

  type Show[A] = cats.Show[A]
  object Show {
    def showA[A]: Show[A] =
      cats.Show.fromToString

    def shows[A](f: A => String): Show[A] =
      cats.Show.show(f)
  }

  type Order[A] = cats.Order[A]
  object Order {
    implicit def apply[A: Order]: Order[A] =
      implicitly[Order[A]]

    def fromScalaOrdering[A](implicit A: Ordering[A]) =
      cats.kernel.Order.fromOrdering[A]
  }
  implicit class OrderSyntax[A](self: Order[A]) {
    def reverseOrder: Order[A] =
      self.reverse

    def contramap[B](f: B => A): Order[B] =
      self.on(f)
  }

  type EitherT[F[_], A, B] = cats.data.XorT[F, A, B]
  val EitherT = cats.data.XorT

  type Monoid[A] = cats.kernel.Monoid[A]
  val Monoid = cats.kernel.Monoid
  implicit class MonoidObjectSyntax(val M: Monoid.type) {
    def instance[A](combine: (A, A) => A, empty: A): Monoid[A] =
      Monoid.instance(combine, empty)
  }

  type ~>[+F[_], -G[_]] = cats.~>[F, G]

  type Contravariant[F[_]] = cats.functor.Contravariant[F]

  val AllInstances = new cats.instances.AllInstances {}
}
