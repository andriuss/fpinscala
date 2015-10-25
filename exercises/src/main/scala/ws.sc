
val l: Either[String, Int] = Left("flower")
val r: Either[String, Int] = Right(12)

l.left.map(_.toString)
l.right.map(_.toString)

r.left.map(_.toString)
r.right.map(_.toString)

def maybeTwice(b: Boolean, i: => Int) = {
  //val i = f()
  //val j = f()
  //val i = ii
  if (b) i+i else 0
}

val x = maybeTwice(true, { println("hi"); 1+41 })
def maybeTwice2(b: Boolean, i: => Int) = {
    println("yo")
    lazy val j=i
    if (b) j+j else 0
}

val x2 = maybeTwice2(true, { println("hi"); 1+41 })

val list: List[Int] = 1 :: 3 :: Nil
val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(3).toList
//0, 1, 1, 2, 3
val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map { n => n._1 + n._2 }
fib.take(10).toList
val xx: Option[(Int, String)] = Some(1, "hi")
xx.map { case (a,b) => println(a + b) }
val twos: Stream[Int] = 2 #:: twos
twos.take(3).toList
(Int.MaxValue - 1).toDouble / 1281479697
List(1, 2, 3).foldRight(List[Int]())((x, acc) => x :: acc)
List.fill(3)(5)
val ls = List(1,2,3)
val oo = List(List(1), Nil, List(2,3))
oo.flatten

