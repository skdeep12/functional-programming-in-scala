object Hello2 extends App {
  val hello = "Hello, World!"
  for {arg <- args} println(arg)
  println(hello)
}
