package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
object FunctorFromMonad {
  implicit def FunctorFromMonad[M[_]](implicit F: Bind[M] with Return[M]): Functor[M] = new Monad[M] {
    override def unit[A](a: => A): M[A] = F.unit(a)
    override def bind[A, B](ma: M[A], f: A => M[B]): M[B] = F.bind(ma, f)
    override def lift[A, B](ma: M[A], f: A => B): M[B] = F.bind[A, B](ma, a => F.unit(f(a)))
  }
}
