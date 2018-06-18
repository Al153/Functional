package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Return[M[_]]{
  def unit[A](a: => A): M[A]
}

