package monads.instances

import monads.typeclasses.Applicable

/**
  * Created by Al on 15/06/2018.
  */
case class Id[A](a: A) {
  def value: A = a
  def lift[B](k: A => B): Id[B] = Id(k(a))
  def bind[B](k: A => Id[B]): Id[B] = k(a)
  def liftA[B](k: Id[A => B]): Id[B] = Id(k.value(a))
}

object Id {
  implicit object IdMonad extends Applicable[Id] {
    override def bind[A, B](ma: Id[A], f: (A) => Id[B]): Id[B] = ma.bind(f)
    override def unit[A](a: => A): Id[A] = Id(a)
    override def lift[A, B](ma: Id[A], f: (A) => B): Id[B] = ma.lift(f)
    override def liftA[A, B](ma: Id[A], mf: Id[(A) => B]): Id[B] = ma.liftA(mf)
  }
}