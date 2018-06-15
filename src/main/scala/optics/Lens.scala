package optics

/**
  * Created by Al on 08/06/2018.
  */
trait Lens[A, B] {
  def get(a: A): B
  def set(a: A)(b: B): A
}

object Lens {
  implicit class LensOps[A, B](l1: Lens[A, B]){
    def compose[C](l2: Lens[B, C]): Lens[A, C] =
      new Lens[A, C] {
        override def get(a: A): C = l2.get(l1.get(a))
        override def set(a: A)(c: C): A = l1.set(a)(l2.set(l1.get(a))(c))
      }
  }
}

case class Angle(a: Angle) extends AnyVal
case class Turtle(location: Point, angle: Angle) {
}

object TurtleAngleLens extends Lens[Turtle, Angle] {
  override def get(t: Turtle): Angle = t.angle
  override def set(t: Turtle)(a: Angle): Turtle = t.copy(angle = a)
}

object TurtlePositionLens extends Lens[Turtle, Point] {
  override def get(t: Turtle): Point = t.location
  override def set(t: Turtle)(p: Point): Turtle = t.copy(location = p)
}

case class Point(x: Float, y: Float)