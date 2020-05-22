
object MyModule {
  def abs(n: Int): Int = 
    if(n < 0 )
      -n
    else
      n
  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is: %d"
    msg.format(x, abs(x))
  }
  
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  } 
}

def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, f: Int, s: Int): Int = {
      if(n == 0) f
      else go(n-1,s,f+s)
    }
    go(n,0,1)
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if(as.length == 1) true
    else if (ordered(as(0),as(1))) go(as.slice(1,as.length),ordered)
    else false
  }
  go(as,ordered)
}


def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  @annotation.tailrec
  def go( i: Int): Int = {
    if(i >= as.length) -1
    else if(p(as(i))) i 
    else go(i+1)
  }
  go(0)
}

def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a,b)
}

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a,b) => f(a)(b)
}

def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}  
