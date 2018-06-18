package monads.typeclasses
import Bind._
import Lift._

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Monad[M[_]] extends Bind[M] with Functor[M]

object Monad {
  def whileM[M[_]](mb: M[Boolean])(mc: M[Unit])(implicit MM: Monad[M]): M[Unit] =
    for {
      b <- mb
      _ <- if (b) MM.bind[Unit, Unit](mc, _ => whileM(mb)(mc)) else MM.unit(())
    } yield ()

  def sequenceM[M[_], A](in: List[M[A]])(implicit MM: Monad[M]): M[List[A]] =
    in.foldRight[M[List[A]]](MM.unit(Nil)){
      case (mx, mxs) =>
        for {
          x <- mx
          xs <- mxs
        } yield x :: xs
    }

  implicit class MonadOps[M[_], A](ma: M[A])(implicit MM: Monad[M]){
    def andThen[B](b: => B): M[B] = ma.flatMap(_ => MM.unit(b))
  }
}