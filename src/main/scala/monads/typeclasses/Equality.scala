package monads.typeclasses

/**
  * Created by Al on 18/06/2018.
  */
trait Equality[A] {
  def equal(a1: A, a2: A): Boolean
}

object Equality {
  implicit class EqualOps[A](a: A)(implicit ea: Equality[A]) {
    def ===(a2: A): Boolean = ea.equal(a, a2)
    def =!=(a2: A): Boolean = !ea.equal(a, a2)
  }
}
