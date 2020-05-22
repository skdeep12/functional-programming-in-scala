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
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_,y) => 1+y)
  }
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match{
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
  }
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc,h) => Cons(h,acc))
  def append[A](a: List[A], b: List[A]): List[A] = foldLeft(reverse(a),b)((x,y) => Cons(y,x))
  def appendUsingFoldRight[A](a: List[A], b: List[A]): List[A] = foldRight(a,b)(Cons(_,_))
  def concat[A](a: List[List[A]]): List[A] = foldLeft(a,List[A]())((a,b) => append(a,b))
  def incrementAllElementsByOne(as: List[Int]) : List[Int] = foldLeft(reverse(as),List[Int]())((b,a) => Cons(a+1,b))
  def toString(as: List[Double]): List[String] = foldRight(as,Nil: List[String])((a,b) => Cons(a.toString(),b))
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as,Nil: List[B])((a,b) => Cons(f(a),b))
}
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def Max(root: Tree[Int]): Int = root match {
    case Leaf(a) => a
    case Branch(l,r) => Max(l) max Max(r)
  }

  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  def map[A,B](root: Tree[A])(f: (A,B) => B): Tree[B] = root match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](root: Tree[A])(f: A => B)(g: (B,B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}


