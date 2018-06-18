package monads.instances

import monads.typeclasses.{Applicative, Filterable}

/**
  * Created by Al on 15/06/2018.
  */

sealed trait Backtrack[+A] {
  def next: (Option[A], Backtrack[A])
  def take(n: Int): List[A]
  def drop(n: Int): Backtrack[A]
  def hasRemaining: Boolean
  def lift[B](f: A => B): Backtrack[B]
  def bind[B](k: A => Backtrack[B]): Backtrack[B]
  def append[B >: A](that: =>  Backtrack[B]): Backtrack[B]
  def interleave[B >: A](that: => Backtrack[B]): Backtrack[B]
  def filter(p: A => Boolean): Backtrack[A]
}

case object LNil extends Backtrack[Nothing] {
  override def next: (Option[Nothing], Backtrack[Nothing]) = (None, LNil)
  override def take(n: Int): List[Nothing] = Nil
  override def drop(n: Int): Backtrack[Nothing] = LNil
  override def lift[B](f: (Nothing) => B): Backtrack[B] = this
  override def hasRemaining: Boolean = false
  override def bind[B](k: (Nothing) => Backtrack[B]): Backtrack[B] = this
  override def append[B >: Nothing](that: => Backtrack[B]): Backtrack[B] = that
  override def interleave[B >: Nothing](that: => Backtrack[B]): Backtrack[B] = that
  override def filter(p: (Nothing) => Boolean): Backtrack[Nothing] = this
}
case class LCons[+A](current: A, remaining: Unit => Backtrack[A]) extends Backtrack[A] {
  private def xf: Backtrack[A] = remaining(())
  override def take(n: Int): List[A] = if (n <= 0) Nil else current :: xf.take(n-1)
  override def drop(n: Int): Backtrack[A] = if (n <= 0) this else xf.drop(n-1)
  override def next: (Option[A], Backtrack[A]) = (Some(current), xf)
  override def hasRemaining: Boolean = false
  override def lift[B](f: (A) => B): Backtrack[B] = LCons(f(current), _ => xf.lift(f))
  override def bind[B](k: (A) => Backtrack[B]): Backtrack[B] = k(current).append(xf.bind(k))
  override def append[B >: A](that: => Backtrack[B]): Backtrack[B] = LCons(current, _ => xf.append(that))
  override def interleave[B >: A](that: => Backtrack[B]): Backtrack[B] =  LCons(current, _ => that.append(xf))
  override def filter(p: (A) => Boolean): Backtrack[A] = if (p(current)) LCons(current, _ => xf.filter(p)) else xf.filter(p)
}

object Backtrack {
  implicit object BacktrackFilterMonad extends Applicative[Backtrack] with Filterable[Backtrack] {
    override def filter[A](fa: Backtrack[A], p: (A) => Boolean): Backtrack[A] = fa.filter(p)
    override def lift[A, B](ma: Backtrack[A], f: (A) => B): Backtrack[B] = ma.lift(f)
    override def unit[A](a: => A): Backtrack[A] = LCons(a, _ => LNil)
    override def bind[A, B](ma: Backtrack[A], f: (A) => Backtrack[B]): Backtrack[B] = ma.bind(f)
    override def liftA[A, B](ma: Backtrack[A], mf: Backtrack[A => B]): Backtrack[B] =
      bind[A, B](ma, a => lift[A => B, B](mf, f => f(a)))
  }


  def choose[A](choices: Seq[A]): Backtrack[A] =
    choices match {
      case x +: xs => LCons(x, _ => choose(xs))
      case _ => LNil
    }

  def decisionPoint[A](left: Backtrack[A], right: Backtrack[A]): Backtrack[A] =
    left.append(right)
}

