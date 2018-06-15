package monads.instances

import monads.typeclasses._

import scala.language.higherKinds






abstract class StateT[S, F[_], A](implicit MF: Monad[F]) {
  def runState(s: S): F[(S, A)]

  import StateT.state
  def lift[B](k: A => B) =
    state[S, F, B] {
      s =>
        val fsa = runState(s)
        MF.lift[(S, A), (S,B)](fsa, {case (ss, a) => (ss, k(a))})
    }

  def bind[B](k: A => StateT[S, F, B]) =
    state[S, F, B] {
      s =>
        val fsa = runState(s)
        MF.bind[(S, A), (S,B)](fsa, {case (ss, a) => k(a).runState(ss)})
    }
}





object StateT {

  def unit[S, F[_], A](a: => A)(implicit MF: Monad[F]): StateT[S, F, A] = state(s => MF.unit(s, a))
  def state[S, F[_], A](f: S => F[(S, A)])(implicit MF: Monad[F]) = new StateT[S, F, A]() {
    override def runState(s: S): F[(S, A)] = f(s)
  }

  implicit def StateMonad[S, F[_]](implicit MF: Monad[F]): Monad[({type T[A] = StateT[S, F, A]})#T] = new Monad[({type T[A] = StateT[S, F, A]})#T] {
    override def unit[A](a: => A): StateT[S, F, A] = StateT.unit(a)
    override def bind[A, B](ma: StateT[S, F, A], f: (A) => StateT[S, F, B]): StateT[S, F, B] = ma.bind(f)
    override def lift[A, B](ma: StateT[S, F, A], f: (A) => B): StateT[S, F, B] = ma.lift(f)
  }
}

