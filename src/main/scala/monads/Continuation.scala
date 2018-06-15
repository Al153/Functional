package monads

import scala.language.higherKinds

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

abstract class StateT[S, F[_], A](implicit MF: Monad[F]) {
  def runState(s: S): (S, F[A])

  import StateT.state
  def lift[B](k: A => B) =
    state[S, F, B] {
      s =>
        val (ss, fa) = runState(s)
        (ss, MF.lift(fa, k))
    }

  def bind[B](k: A => StateT[S, F, B]) =
    state[S, F, B] {
      s =>
        val (ss, fa) = runState(s)
        k(fa).runState(ss)
    }
}

case class Id[A](a: A) extends AnyVal {
  def value: A = a
  def lift[B](k: A => B): Id[B] = Id(k(a))
  def bind[B](k: A => Id[B]): Id[B] = k(a)
  def liftA[B](k: Id[A => B]): Id[B] = Id(k.value(a))
}

object Id {
  implicit object IdMonad extends Applicable[Id] {
    override def bind[A, B](ma: Id[A], f: (A) => Id[B]): Id[B] = ma.bind(f)
    override def unit[A](a: => A): Id[A] = Id(a)
    override def lift[A, B](ma: Id[A], f: (A) => B): Id[B] = ma.lift(f)
    override def liftA[A, B](ma: Id[A], mf: Id[(A) => B]): Id[B] = ma.liftA(mf)
  }
}

object StateT {

  def unit[S, F[_], A](a: => A)(implicit MF: Monad[F]): StateT[S, F, A] = state(s => (s, MF.unit(a)))
  def state[S, F[_], A](f: S => (S, F[A]))(implicit MF: Monad[F]) = new StateT[S, F, A]() {
    override def runState(s: S): (S, F[A]) = f(s)
  }

  implicit def StateMonad[S, F[_]](implicit MF: Monad[F]): Monad[({type T[A] = StateT[S, F, A]})#T] = new Monad[({type T[A] = StateT[S, F, A]})#T] {
    override def unit[A](a: => A): StateT[S, F, A] = StateT.unit(a)
    override def bind[A, B](ma: StateT[S, F, A], f: (A) => StateT[S, F, B]): StateT[S, F, B] = ma.bind(f)
    override def lift[A, B](ma: StateT[S, F, A], f: (A) => B): StateT[S, F, B] = ma.lift(f)
  }
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


trait Bind[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]
}

trait Mappable[M[_]] {
  def lift[A, B](ma: M[A], f: A => B): M[B]
}

trait Return[M[_]]{
  def unit[A](a: => A): M[A]
}

trait Apply[M[_]]{
  def liftA[A, B](ma: M[A], mf: M[A => B]): M[B]
}

trait Functor[M[_]] extends Mappable[M] with Return[M]
trait Monad[M[_]] extends Bind[M] with Functor[M]
trait Applicable[M[_]] extends Monad[M] with Apply[M]

object FunctorFromMonad {
  implicit def FunctorFromMonad[M[_]](implicit F: Bind[M] with Return[M]): Functor[M] = new Monad[M] {
    override def unit[A](a: => A): M[A] = F.unit(a)
    override def bind[A, B](ma: M[A], f: A => M[B]) = F.bind(ma, f)
    override def lift[A, B](ma: M[A], f: A => B) = F.bind(ma, a => F.unit(f(a)))
  }
}

object Monad {
  implicit class BindOps[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.bind(ma, f)
  }

  implicit class FunctorOps[M[_], A](ma: M[A])(implicit M: Functor[M]) {
    def map[B](f: A => B): M[B] = M.lift(ma, f)
  }
}