package monads.instances

import monads.typeclasses.Monad

/**
  * Created by Al on 13/06/2018.
  */
trait Continuation[R, +A] {
  def apply(f: A => R): R

  import Continuation.continuation

  def lift[B](k: A => B) =
    continuation[R, B](z => apply(z compose k))

  def bind[B](k: A => Continuation[R, B]) =
    continuation[R, B](z => apply(k(_)(z)))
}

object Continuation {
  def continuation[R, A](g: (A => R) => R) = new Continuation[R, A] {
    def apply(f: A => R) = g(f)
  }

  def unit[R] = new {
    def apply[A](a: A) = continuation[R, A](f => f(a))
  }

  def callcc[R, A, B](f: (A => Continuation[R, B]) => Continuation[R, A]) =
    continuation[R, A](k => f(a => continuation(x => k(a)))(k))

  implicit def ContinuationMonad[R]: Monad[({type T[A] = Continuation[R, A]})#T] = new Monad[({type T[A] = Continuation[R, A]})#T] {
    override def unit[A](a: => A) = Continuation.unit(a)
    override def bind[A, B](ma: Continuation[R, A], f: (A) => Continuation[R, B]): Continuation[R, B] = ma.bind(f)
    override def lift[A, B](ma: Continuation[R, A], f: (A) => B) = ma.lift(f)
  }
}