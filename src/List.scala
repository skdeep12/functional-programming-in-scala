sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail on empty list")
    case Cons(_,t) => t
  }
  def setHead[A](l : List[A], v: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(v,t)
  }
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => if(n > 0) sys.error("not enough elements to drop") else Nil
    case Cons(_, t) => if(n > 0) drop(t,n-1) else l
  }
}
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

var y = List(1,2,3,4)
val ans = List.drop(y,2)
