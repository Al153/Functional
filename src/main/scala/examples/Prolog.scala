package examples

import monads.instances.StateT._
import monads.instances.{Empty, Just, Maybe, StateT}
import monads.typeclasses._
import Monad._
import Lift._
import Bind._

import scala.collection.MapLike


/**
  * Created by Al on 16/06/2018.
  */
object Prolog {

}

sealed trait Term
case class BoolTerm(b: Boolean) extends Term
case class IntTerm(i: Int) extends Term
case class Variable(s: VarName) extends Term
case class Compound(name: CompoundName, terms: List[Term]) extends Term

case class VarName(n: String) extends AnyVal
case class CompoundName(n: String) extends AnyVal
case class Substitution(m: Map[VarName, Term]) {
  def add(n: VarName, term: Term): Substitution = copy(m + (n -> term))
}

object Substitution {
  def empty = Substitution(Map())
}

object Term {
  type SubStore[A] = StateT[Substitution, Maybe, A]

  implicit class MapOps[A, B](a: A) {
    def in[M <: MapLike[A, B, M]](m: M): Boolean = m.contains(a)
  }

  def mgu(t1: Term, t2: Term): SubStore[Term] = {
    def aux(t1: Term, t2: Term): SubStore[Term] =
      t1 match {
          // todo: check for t2 variable
        case BoolTerm(b) =>
          t2 match {
            case BoolTerm(c) if b == c => constant(Just(t1))
            case _ => constant[Substitution, Maybe, Term](Empty)
          }

        case IntTerm(i) =>
          t2 match {
            case IntTerm(j) if i == j => constant(Just(t1))
            case _ => constant[Substitution, Maybe, Term](Empty)
          }
        case Variable(n) =>
          t2 match {
            case BoolTerm(b) => 
              new MonadOps(writeUnit[Substitution, Maybe](s => s.add(n, t2))).andThen(t2)
            case IntTerm(i) => ???
            case Variable(s) => ???
            case Compound(name, terms) => ???
          }

        case Compound(name, terms) =>
          t2 match {
            case Compound(name2, terms2) if name2 == name && terms2.length == terms.length =>
              Monad.sequenceM[SubStore, Term](for {
                (t1, t2) <- terms.zip(terms2)
                r <- aux(t1, t2)
              } yield r) map (l => Compound(name, l))
            case _ => constant[Substitution, Maybe, Term](Empty)

          }
      }


    for {
      r1 <- reifyTerm(t1)
      r2 <- reifyTerm(t2)
      res <- aux(r1, r2)
    } yield res
  }

  def reifyTerm(t1: Term): StateT[Substitution, Maybe, Term] = readState {
    s =>
     def lookup(t: Term): Term =
        t match {
          case Variable(name) => s.m.get(name).map(lookup).getOrElse(t)
          case Compound(name, terms) => Compound(name, terms.map(lookup))
      }

      Just(lookup(t1))
  }
}
