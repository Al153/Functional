package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Lift[M[_]] {
  def lift[A, B](ma: M[A], f: A => B): M[B]
}

object Lift {
  implicit class LiftOps[M[_], A](ma: M[A])(implicit M: Lift[M]) {
    def map[B](f: A => B): M[B] = M.lift(ma, f)
  }
}