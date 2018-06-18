package monads

import monads.typeclasses.Monad
import monads.typeclasses.Bind._

import scala.language.higherKinds

/**
  * Created by Al on 18/06/2018.
  */
abstract class MonadTest[M[_]](implicit MM: Monad[M]) {
  private def leftId[A, B](a: A, f: A => M[B]) =
    assert((MM.unit(a) >>= f) == f(a), s"Fails left identity at $f $a")

  private def rightId[A](m: M[A]) =
    assert((m >>= MM.unit) == m, s"Fails right identity at $m")

  private def associativity[A, B, C](ma: M[A], f: A => M[B], g: B => M[C]) =
    assert()
}
