package monads.instances

import monads.typeclasses.{Applicable, Filterable}

/**
  * Created by Al on 15/06/2018.
  */
trait Backtrack[A] {
  def runBacktrack: BacktrackResult[A]
  def lift[B]()
  def bind[B](k: A => Backtrack[B]): Backtrack[B] = ???

}

sealed trait BacktrackResult[A] {
  def next: (Option[A], BacktrackResult[A])
  def take(n: Int): List[A]
  def drop(n: Int): BacktrackResult[A]
  def hasRemaining: Boolean
  def map[B](f: A => B): BacktrackResult[B]
}

case object EmptyBacktrack extends BacktrackResult[Nothing] {
  override def next: (Option[Nothing], BacktrackResult[Nothing]) = (None, EmptyBacktrack)
  override def take(n: Int): List[Nothing] = Nil
  override def drop(n: Int): BacktrackResult[Nothing] = EmptyBacktrack
  override def map[B](f: (Nothing) => B): BacktrackResult[B] = ???
}
case class BacktrackImpl[A](current: A, remaining: Unit => BacktrackResult[A]) extends BacktrackResult[A] {
  override def take(n: Int): List[A] = if (n <= 0) Nil else current :: remaining(()).take(n-1)
  override def drop(n: Int): BacktrackResult[A] = ???
  override def next: (Option[A], BacktrackResult[A]) = (Some(current), remaining(()))
}

object Backtrack {
  implicit object BacktrackFilterMonad extends Applicable[Backtrack] with Filterable[Backtrack] {
    override def filter[A](fa: Backtrack[A], p: (A) => Boolean): Backtrack[A] = ???

    override def lift[A, B](ma: Backtrack[A], f: (A) => B): Backtrack[B] = ???

    override def unit[A](a: => A): Backtrack[A] = ???

    override def bind[A, B](ma: Backtrack[A], f: (A) => Backtrack[B]): Backtrack[B] = ???

    override def liftA[A, B](ma: Backtrack[A], mf: Backtrack[(A) => B]): Backtrack[B] = ???
  }
}

