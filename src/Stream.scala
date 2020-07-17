import scala.collection.immutable.Stream.Empty

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](hd: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def toList[A]: List[A] = this match {
    case Cons(hd, tl ) => hd()::tl.toList
    case _ => Nil
  }
}
