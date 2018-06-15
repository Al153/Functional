package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Bind[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]
}
