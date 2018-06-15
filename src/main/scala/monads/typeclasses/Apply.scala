package monads.typeclasses

/**
  * Created by Al on 15/06/2018.
  */
trait Apply[M[_]]{
  def liftA[A, B](ma: M[A], mf: M[A => B]): M[B]
}
