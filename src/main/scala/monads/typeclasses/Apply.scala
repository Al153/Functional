package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Apply[M[_]]{
  def liftA[A, B](ma: M[A], mf: M[A => B]): M[B]
}

object Apply {
  implicit class ApplyOps[M[_], A](ma: M[A])(implicit MA: Apply[M]) {

  }
}