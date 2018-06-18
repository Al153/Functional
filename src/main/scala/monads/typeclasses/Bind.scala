package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Bind[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]
}

object Bind {
  implicit class BindOps[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.bind(ma, f)
    def >>=[B](f: A => M[B]): M[B] = flatMap(f)
    def >>[B](mb: => M[B]): M[B] = flatMap(_ => mb)
  }
}