package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Functor[M[_]] extends Mappable[M] with Return[M]
