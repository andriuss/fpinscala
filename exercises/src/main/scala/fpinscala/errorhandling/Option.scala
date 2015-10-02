package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
/*    val m: Option[Some[A]] = this map (Some(_))
    m.getOrElse(ob)*/

    this match {
      case Some(v) => Some(v)
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map(bb => f(aa,bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Some(x) :: xs => sequence(xs) flatMap (ls => Some(x :: ls))
    case Nil => Some(Nil)
    case _ => None
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case x :: xs => traverse(xs)(f) flatMap(ls => f(x) map (_ :: ls))
    case Nil => Some(Nil)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}

object TestOption {
  import fpinscala.errorhandling.Option._

  def main(args: Array[String]): Unit = {

    val o1: Option[String] = Some("1")
    val o2: Option[String] = None

    println("o1 map: " + o1.map(_ + "_"))
    println("o2 map: " + o2.map(_ + "_"))

    println("o1 getOrElse: " + o1.getOrElse("_"))
    println("o2 getOrElse: " + o2.getOrElse("_"))

    println("o1 flatMap: " + o1.flatMap(x => Some("_")))
    println("o2 flatMap: " + o2.flatMap(x => Some("_")))

    println("o1 orElse: " + o1.orElse(Some("_")))
    println("o2 orElse: " + o2.orElse(Some("_")))

    println("o1 filter: " + o1.filter(_ == "1"))
    println("o1 filter: " + o1.filter(_ == "2"))
    println("o2 filter: " + o2.filter(_ == "1"))

    val list = List(1.0,2.0,3.0)
    println("mean list: " + mean(list))
    println("mean list: " + mean(List()))

    println("variance list: " + variance(list))
    println("variance list: " + variance(List()))

    println("map2: " + map2(Some(1), Some(2))(_ + _))
    println("map2: " + map2(Some("1"), Some("2"))(_ + _))
    println("map2: " + map2[Int, Int, Int](Some(1), None)(_ + _))

    println("sequence: " + sequence(List(Some("1"), Some("2"))))
    println("sequence: " + sequence(List(Some("1"), None, Some("2"))))
    println("sequence: " + sequence(List(Some("1"))))
    println("sequence: " + sequence(List(None)))
    println("sequence: " + sequence(Nil))

    def f(x: String): Option[Int] = if (x == "1" || x == "2") Some(x.toInt) else None
    println("traverse: " + traverse[String, Int](List("1", "2"))(f))
    println("traverse: " + traverse[String, Int](List("1", "2", "3"))(f))
    println("traverse: " + traverse[String, Int](List("1"))(f))
    println("traverse: " + traverse[String, Int](List())(f))
    println("traverse: " + traverse[String, Int](List("3"))(f))

    println("sequence2: " + sequence2(List(Some("1"), Some("2"))))
    println("sequence2: " + sequence2(List(Some("1"), None, Some("2"))))
    println("sequence2: " + sequence2(List(Some("1"))))
    println("sequence2: " + sequence2(List(None)))
    println("sequence2: " + sequence2(Nil))

  }
}