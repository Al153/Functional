package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Filterable[F[_]] {
  def filter[A](fa: F[A], p: A => Boolean): F[A]
}

object Filterable {
  implicit class FilterableOps[F[_], A](fa: F[A])(implicit FF: Filterable[F]) {
    def withFilter(p: A => Boolean): F[A] = FF.filter(fa, p)
  }
}
