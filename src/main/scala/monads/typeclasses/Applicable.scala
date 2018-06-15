package monads.typeclasses

import scala.language.higherKinds

/**
  * Created by Al on 15/06/2018.
  */
trait Applicable[M[_]] extends Monad[M] with Apply[M]
