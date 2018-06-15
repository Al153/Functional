package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Mappable[M[_]] {
  def lift[A, B](ma: M[A], f: A => B): M[B]
}
