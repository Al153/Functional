package monads.instances

import monads.typeclasses._

import scala.language.{higherKinds, reflectiveCalls}






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
  def constant[S, F[_], A](fa: => F[A])(implicit MF: Monad[F]): StateT[S, F, A] =
    readState(_ => fa)

  def unit[S, F[_], A](a: => A)(implicit MF: Monad[F]): StateT[S, F, A] = state(s => MF.unit(s, a))
  def state[S, F[_], A](f: S => F[(S, A)])(implicit MF: Monad[F]) = new StateT[S, F, A]() {
    override def runState(s: S): F[(S, A)] = f(s)
  }

  def readState[S, F[_], A](read: S => F[A])(implicit MF: Monad[F]): StateT[S, F, A] = state {
    s => MF.lift[A, (S, A)](read(s), (s, _))
  }

  def readUnit[S, F[_], A](read: S => A)(implicit MF: Monad[F]): StateT[S, F, A] = state {
    s => MF.lift[A, (S, A)](MF.unit(read(s)), (s, _))
  }

  def writeState[S, F[_]](write: S => F[S])(implicit MF: Monad[F]): StateT[S, F, Unit] = state {
    s => MF.lift[S, (S, Unit)](write(s), (_, ()))
  }

  def writeUnit[S, F[_]](write: S => S)(implicit MF: Monad[F]): StateT[S, F, Unit] = state {
    s => MF.lift[S, (S, Unit)](MF.unit(write(s)), (_, ()))
  }


  // upgrade an ID state to a more interesting one.
  def liftState[S, F[_], A](sia: StateT[S, Id, A])(implicit MF: Monad[F]): StateT[S, F, A] = state[S, F, A] {
    s => MF.unit(sia.runState(s).a)
  }

  implicit def StateMonad[S, F[_]](implicit MF: Monad[F]): Monad[({type T[A] = StateT[S, F, A]})#T] = new Monad[({type T[A] = StateT[S, F, A]})#T] {
    override def unit[A](a: => A): StateT[S, F, A] = StateT.unit(a)
    override def bind[A, B](ma: StateT[S, F, A], f: (A) => StateT[S, F, B]): StateT[S, F, B] = ma.bind(f)
    override def lift[A, B](ma: StateT[S, F, A], f: (A) => B): StateT[S, F, B] = ma.lift(f)
  }


}

