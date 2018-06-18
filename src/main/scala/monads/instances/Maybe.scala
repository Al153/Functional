package monads.instances

import monads.typeclasses.{Applicative, Filterable, Monad}

/**
  * Created by Al on 16/06/2018.
  */
sealed trait Maybe[+A] {
  def fold[B](b: => B, f: A => B): B
}

case class Just[A](a: A) extends Maybe[A] {
  override def fold[B](b: B, f: (A) => B): B = f(a)
}

case object Empty extends Maybe[Nothing] {
  override def fold[B](b: B, f: (Nothing) => B): B = b
}

object Maybe {
  def apply[A](a: A): Maybe[A] = Just(a)

  implicit object MaybeMonad extends Monad[Maybe] with Applicative[Maybe] with Filterable[Maybe] {
    override def filter[A](fa: Maybe[A], p: (A) => Boolean): Maybe[A] =
      fa.fold[Maybe[A]](Empty, a => if (p(a)) fa else Empty)

    override def lift[A, B](ma: Maybe[A], f: (A) => B): Maybe[B] =
      ma.fold[Maybe[B]](Empty, a => Just(f(a)))

    override def bind[A, B](ma: Maybe[A], f: (A) => Maybe[B]): Maybe[B] =
      ma.fold[Maybe[B]](Empty, a => f(a))

    override def liftA[A, B](ma: Maybe[A], mf: Maybe[(A) => B]): Maybe[B] =
      bind[A, B](ma, a => lift[A => B, B](mf, f => f(a)))

    override def unit[A](a: => A): Maybe[A] = Just(a)
  }
}