package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Monad[M[_]] extends Bind[M] with Functor[M]

object Monad {
  implicit class BindOps[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.bind(ma, f)
  }

  implicit class FunctorOps[M[_], A](ma: M[A])(implicit M: Functor[M]) {
    def map[B](f: A => B): M[B] = M.lift(ma, f)
  }
}